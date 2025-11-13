use std::fs;

use crate::{
    clif::ClifBackend,
    mir::{Inst, TermInst, Typ, Val},
    mirbuild::MirBuilder,
};

mod clif;
mod mir;
mod mirbuild;

fn host_triple() -> String {
    target_lexicon::Triple::host().to_string()
}

/// Compiler for the Dolphin language
#[derive(argh::FromArgs)]
struct Args {
    /// the target triple to compile for
    #[argh(option, default = "host_triple()")]
    target_triple: String,
    /// where to write the output to
    #[argh(option, short = 'o', default = "String::from(\"output.o\")")]
    output: String,
}

fn main() {
    let args = argh::from_env::<Args>();
    let mut translator = ClifBackend::new(&args.output, &args.target_triple);
    let mut function_translator = translator.start_function("main");

    let mut mir = MirBuilder::new();

    let (th_id, mut th_builder) = mir.block();
    let (el_id, mut el_builder) = mir.block();

    let (entry_id, mut builder) = mir.block();
    let i1 = mir.reg("i1");
    builder.emit(Inst::Imm(i1, Val::I8(1)));
    let my_var = mir.place("my_var");
    builder.emit(Inst::Alloca(my_var, Typ::I8));
    builder.emit(Inst::Store(my_var, Typ::I8, i1));
    let entry_bb = builder.term(TermInst::If {
        cond: i1,
        th: th_id,
        el: el_id,
    });
    function_translator.translate(entry_id, entry_bb);

    let i2 = mir.reg("i2");
    th_builder.emit(Inst::Imm(i2, Val::I8(1)));
    let my_var_value = mir.reg("my_var1");
    th_builder.emit(Inst::Load(my_var_value, Typ::I8, my_var));
    let r2 = mir.reg("r2");
    th_builder.emit(Inst::Add(r2, Typ::I8, i2, my_var_value));
    let th_bb = th_builder.term(TermInst::Ret(Typ::I8, r2));
    function_translator.translate(th_id, th_bb);

    el_builder.emit(Inst::Imm(i2, Val::I8(2)));
    let el_bb = el_builder.term(TermInst::Ret(Typ::I8, i2));
    function_translator.translate(el_id, el_bb);

    let func_id = function_translator.finish();
    translator.finish_function(func_id);
    let product = translator.finish();

    fs::write(&args.output, product).unwrap();
}
