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

    // Entry block: if (1+1) { th_bb } else { el_bb }
    let (entry_id, mut builder) = mir.block();
    let ciao = mir.reg("ciao");
    builder.emit(Inst::Imm(ciao, Val::I8(1)));
    let mondo = mir.reg("mondo");
    builder.emit(Inst::Imm(mondo, Val::I8(1)));
    let risultato = mir.reg("risultato");
    builder.emit(Inst::Add(risultato, Typ::I8, ciao, mondo));
    let entry_bb = builder.term(TermInst::If {
        cond: risultato,
        th: th_id,
        el: el_id,
    });
    function_translator.translate(entry_id, entry_bb);

    let res = mir.reg("res");
    th_builder.emit(Inst::Imm(res, Val::I8(1)));
    let th_bb = th_builder.term(TermInst::Ret(Typ::I8, res));
    function_translator.translate(th_id, th_bb);

    el_builder.emit(Inst::Imm(res, Val::I8(2)));
    let el_bb = el_builder.term(TermInst::Ret(Typ::I8, res));
    function_translator.translate(el_id, el_bb);

    let func_id = function_translator.finish();
    translator.finish_function(func_id);
    let product = translator.finish();

    fs::write(&args.output, product).unwrap();
}
