use std::{collections::HashMap, sync::Arc};

use cranelift::prelude::*;
use cranelift_codegen::isa::TargetIsa;
use cranelift_module::{FuncId, Linkage, Module, default_libcall_names};
use cranelift_object::{ObjectBuilder, ObjectModule};
use target_lexicon::Triple;

use crate::mir::{BasicBlock, BlockId, Inst, Place, Reg, TermInst, Val};

pub struct ClifBackend {
    isa: Arc<dyn TargetIsa>,
    module: ObjectModule,
    codegen_ctx: codegen::Context,
    function_ctx: FunctionBuilderContext,

    regs: HashMap<Reg, Value>,
    vars: HashMap<Place, Variable>,
    bblocks: HashMap<BlockId, Block>,
}

impl ClifBackend {
    pub fn new(translation_unit: &str, target: Triple) -> Self {
        let isa = {
            let mut builder = settings::builder();
            builder.set("opt_level", "speed_and_size").unwrap();
            builder.enable("is_pic").unwrap();
            let flags = settings::Flags::new(builder);

            isa::lookup(target).unwrap().finish(flags).unwrap()
        };

        let builder = ObjectBuilder::new(isa.clone(), translation_unit, default_libcall_names());
        let module = ObjectModule::new(builder.unwrap());

        let ctx = codegen::Context::new();
        let fctx = FunctionBuilderContext::new();

        Self {
            isa,
            module,
            codegen_ctx: ctx,
            function_ctx: fctx,
            regs: HashMap::new(),
            bblocks: HashMap::new(),
            vars: HashMap::new(),
        }
    }

    pub fn start_function<'a>(&'a mut self, name: &str) -> ClifTranslator<'a> {
        let signature = Signature {
            call_conv: self.isa.default_call_conv(),
            params: vec![],
            returns: vec![AbiParam::new(types::I8)],
        };

        let func_id = self
            .module
            .declare_function(name, Linkage::Export, &signature)
            .unwrap();

        let builder = FunctionBuilder::new(&mut self.codegen_ctx.func, &mut self.function_ctx);
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

    pub fn finish_function<'a>(&'a mut self, func_id: FuncId) {
        self.module
            .define_function(func_id, &mut self.codegen_ctx)
            .unwrap();
    }

    pub fn finish(self) -> Vec<u8> {
        self.module
            .finish()
            .emit()
            .expect("Failed to emit object file")
    }
}

pub struct ClifTranslator<'a> {
    isa: Arc<dyn TargetIsa>,
    func_id: FuncId,
    builder: FunctionBuilder<'a>,

    regs: &'a mut HashMap<Reg, Value>,
    vars: &'a mut HashMap<Place, Variable>,
    bblocks: &'a mut HashMap<BlockId, Block>,
}

impl<'a> ClifTranslator<'a> {
    fn ins<'b>(&'b mut self) -> cranelift::frontend::FuncInstBuilder<'b, 'a> {
        self.builder.ins()
    }

    fn block(&mut self, id: BlockId) -> Block {
        *self
            .bblocks
            .entry(id)
            .or_insert_with(|| self.builder.create_block())
    }

    pub fn translate(&mut self, bb: BasicBlock) {
        let block = self.block(bb.id);
        self.builder.switch_to_block(block);
        self.builder.seal_block(block);

        for inst in bb.insts {
            match inst {
                Inst::Comment(_) => {}
                Inst::Alloca(place, typ) => {
                    let var = self.builder.declare_var(typ.into());
                    self.vars.insert(place, var);
                }
                Inst::Store(place, _typ, reg) => {
                    self.builder.def_var(self.vars[&place], self.regs[&reg]);
                }
                Inst::Load(reg, _typ, place) => {
                    let val = self.builder.use_var(self.vars[&place]);
                    self.regs.insert(reg, val);
                }
                Inst::Imm(reg, val) => {
                    let (typ, val) = match val {
                        Val::I8(val) => (types::I8, val as i64),
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
        };
    }

    #[must_use = "Pass the FuncId to `ClifTranslator::finish_function`"]
    pub fn finish(mut self) -> FuncId {
        let mut str = String::new();
        codegen::write_function(&mut str, &self.builder.func).unwrap();
        println!("{str}");

        codegen::verify_function(&self.builder.func, self.isa.as_ref()).expect("Verifier error");
        self.builder.seal_all_blocks();
        self.builder.finalize();

        self.func_id
    }
}
