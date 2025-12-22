use std::{collections::HashMap, sync::Arc};

use cranelift::{
    codegen::{
        self,
        ir::{AbiParam, Block, InstBuilder, Signature, Value, condcodes::IntCC, entities, types},
        isa::{TargetIsa, lookup},
        settings::{self, Configurable},
    },
    frontend,
    module::{FuncId, Linkage, Module, default_libcall_names},
    object::{ObjectBuilder, ObjectModule},
};
use target_lexicon::Triple;

use crate::{
    Args,
    lowering::Event,
    mir::{BasicBlock, BlockId, Inst, Place, Reg, TermInst, Val},
};

fn print_event(e: &Event) {
    match e {
        Event::Function(s) => println!("Start function {s}"),
        Event::Block(basic_block) => println!("Emit basic block {}", basic_block.id.0),
        Event::DisableAutoSeal => println!("Disable auto-seal"),
        Event::EnableAutoSeal => println!("Enable auto-seal"),
    }
}

pub fn handle_events(args: &Args, events: Vec<Event>) -> Vec<u8> {
    let mut clif = ClifBackend::new(&args.output, args.target_triple.clone());
    let mut iter = events.into_iter().peekable();
    while let Some(ev) = iter.next() {
        print_event(&ev);

        if let Event::Function(name) = &ev {
            let mut clif_func = clif.start_function(name);
            let mut auto_seal = true;

            while !matches!(iter.peek(), Some(Event::Function(_)) | None) {
                match iter.next().unwrap() {
                    Event::Function(_) => unreachable!(),
                    Event::Block(ref basic_block) => clif_func.translate(basic_block, auto_seal),
                    Event::DisableAutoSeal => {
                        assert!(auto_seal);
                        auto_seal = false
                    }
                    Event::EnableAutoSeal => {
                        assert!(!auto_seal);
                        clif_func.seal_all();
                        auto_seal = true
                    }
                }
            }

            let id = clif_func.finish();
            clif.finish_function(id);
        }
    }

    clif.finish()
}

struct ClifBackend {
    isa: Arc<dyn TargetIsa>,
    module: ObjectModule,
    codegen_ctx: codegen::Context,
    function_ctx: frontend::FunctionBuilderContext,

    regs: HashMap<Reg, entities::Value>,
    vars: HashMap<Place, frontend::Variable>,
    bblocks: HashMap<BlockId, entities::Block>,
}

impl ClifBackend {
    fn new(translation_unit: &str, target: Triple) -> Self {
        let isa = {
            let mut builder = settings::builder();
            builder.set("opt_level", "speed_and_size").unwrap();
            builder.enable("is_pic").unwrap();
            let flags = settings::Flags::new(builder);

            lookup(target).unwrap().finish(flags).unwrap()
        };

        let builder = ObjectBuilder::new(isa.clone(), translation_unit, default_libcall_names());
        let module = ObjectModule::new(builder.unwrap());
        let ctx = codegen::Context::new();

        Self {
            isa,
            module,
            codegen_ctx: ctx,
            function_ctx: Default::default(),
            regs: HashMap::new(),
            bblocks: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    fn start_function<'a>(&'a mut self, name: &str) -> ClifTranslator<'a> {
        let signature = Signature {
            call_conv: self.isa.default_call_conv(),
            params: vec![],
            returns: vec![AbiParam::new(types::I64)],
        };

        let func_id = self
            .module
            .declare_function(name, Linkage::Export, &signature)
            .unwrap();

        let builder =
            frontend::FunctionBuilder::new(&mut self.codegen_ctx.func, &mut self.function_ctx);
        builder.func.clear();
        builder.func.signature = signature;

        ClifTranslator {
            func_id,
            builder,
            isa: Arc::clone(&self.isa),
            regs: &mut self.regs,
            vars: &mut self.vars,
            bblocks: &mut self.bblocks,
        }
    }

    fn finish_function(&mut self, func_id: FuncId) {
        self.module
            .define_function(func_id, &mut self.codegen_ctx)
            .unwrap();
    }

    fn finish(self) -> Vec<u8> {
        self.module
            .finish()
            .emit()
            .expect("Failed to emit object file")
    }
}

pub struct ClifTranslator<'a> {
    isa: Arc<dyn TargetIsa>,
    func_id: FuncId,
    builder: frontend::FunctionBuilder<'a>,

    regs: &'a mut HashMap<Reg, Value>,
    vars: &'a mut HashMap<Place, frontend::Variable>,
    bblocks: &'a mut HashMap<BlockId, Block>,
}

impl<'a> ClifTranslator<'a> {
    fn ins<'b>(&'b mut self) -> frontend::FuncInstBuilder<'b, 'a> {
        self.builder.ins()
    }

    fn block(&mut self, id: BlockId) -> Block {
        *self
            .bblocks
            .entry(id)
            .or_insert_with(|| self.builder.create_block())
    }

    fn seal_all(&mut self) {
        self.builder.seal_all_blocks();
    }

    fn translate(&mut self, bb: &BasicBlock, seal: bool) {
        let block = self.block(bb.id);
        self.builder.switch_to_block(block);
        if seal {
            self.builder.seal_block(block);
        }

        for inst in bb.insts.iter().copied() {
            match inst {
                Inst::Comment(_) => {}
                Inst::Alloca(place, typ) => {
                    if typ.is_storable() {
                        let var = self.builder.declare_var(typ.into());
                        self.vars.insert(place, var);
                    }
                }
                Inst::Store(place, typ, reg) => {
                    if typ.is_storable() {
                        self.builder.def_var(self.vars[&place], self.regs[&reg]);
                    }
                }
                Inst::Load(reg, typ, place) => {
                    if typ.is_storable() {
                        let val = self.builder.use_var(self.vars[&place]);
                        self.regs.insert(reg, val);
                    }
                }
                Inst::Imm(reg, val) => {
                    let (typ, val) = match val {
                        Val::I8(val) => (types::I8, val as i64),
                        Val::I64(val) => (types::I64, val),
                        Val::False => todo!(),
                        Val::True => todo!(),
                    };

                    let res = self.ins().iconst(typ, val);
                    self.regs.insert(reg, res);
                }
                Inst::Add(reg, _typ, reg1, reg2) => {
                    let reg1 = self.regs[&reg1];
                    let reg2 = self.regs[&reg2];
                    let res = self.ins().iadd(reg1, reg2);
                    self.regs.insert(reg, res);
                }
                Inst::Lt(reg, _typ, reg1, reg2) => {
                    let reg1 = self.regs[&reg1];
                    let reg2 = self.regs[&reg2];
                    let res = self.ins().icmp(IntCC::SignedLessThan, reg1, reg2);
                    self.regs.insert(reg, res);
                }
            }
        }

        match bb.term {
            TermInst::Ret(_, reg) => {
                let value = self.regs[&reg];
                self.ins().return_(&[value])
            }
            TermInst::If { cond, th, el } => {
                let c = self.regs[&cond];
                let th = self.block(th);
                let el = self.block(el);
                self.ins().brif(c, th, &[], el, &[])
            }
            TermInst::Jmp(block_id) => {
                let bb = self.block(block_id);
                self.ins().jump(bb, &[])
            }
        };
    }

    #[must_use = "Pass the FuncId to `ClifTranslator::finish_function`"]
    fn finish(mut self) -> FuncId {
        let mut str = String::new();
        codegen::write_function(&mut str, self.builder.func).unwrap();
        println!("{str}");

        codegen::verify_function(self.builder.func, self.isa.as_ref()).expect("Verifier error");
        self.builder.seal_all_blocks();
        self.builder.finalize();

        self.func_id
    }
}
