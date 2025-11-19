use std::{fs, path::Path, str::FromStr};

use target_lexicon::Triple;
use tempfile::NamedTempFile;

use crate::{
    clif::ClifBackend,
    linker::link,
    mir::{Inst, TermInst, Typ, Val},
    mirbuild::MirBuilder,
};

mod clif;
mod linker;
mod mir;
mod mirbuild;
mod mirprint;

fn parse_triple(s: &str) -> Result<Triple, String> {
    Triple::from_str(s).map_err(|e| format!("{e}"))
}

/// Compiler for the Dolphin language
#[derive(argh::FromArgs)]
struct Args {
    /// the target triple to compile for
    #[argh(option, default = "Triple::host()", from_str_fn(parse_triple))]
    target_triple: Triple,
    /// where to write the output to
    #[argh(option, short = 'o', default = "String::from(\"output\")")]
    output: String,
}

fn main() {
    let args = argh::from_env::<Args>();
    let mut translator = ClifBackend::new(&args.output, args.target_triple.clone());
    let mut function_translator = translator.start_function("main");

    let mut mir = MirBuilder::new();

    let mut builder = mir.block();
    let mut th_builder = mir.block();
    let mut el_builder = mir.block();
    let my_var = mir.place("my_var");
    println!("== MIR ==");

    {
        let i1 = mir.reg("i1");
        builder.comment("test");
        builder.emit(Inst::Imm(i1, Val::I8(0)));
        builder.emit(Inst::Alloca(my_var, Typ::I8));
        builder.emit(Inst::Store(my_var, Typ::I8, i1));
        let entry_bb = builder.term(TermInst::If {
            cond: i1,
            th: th_builder.id(),
            el: el_builder.id(),
        });
        mir.print_block(&entry_bb);
        function_translator.translate(entry_bb);
    }

    {
        let i2 = mir.reg("i2");
        th_builder.emit(Inst::Imm(i2, Val::I8(10)));
        let my_var_value = mir.reg("my_var1");
        th_builder.emit(Inst::Load(my_var_value, Typ::I8, my_var));
        let r2 = mir.reg("r2");
        th_builder.emit(Inst::Add(r2, Typ::I8, i2, my_var_value));
        let th_bb = th_builder.term(TermInst::Ret(Typ::I8, r2));
        mir.print_block(&th_bb);
        function_translator.translate(th_bb);
    }

    {
        let i3 = mir.reg("i3");
        el_builder.emit(Inst::Imm(i3, Val::I8(2)));
        let el_bb = el_builder.term(TermInst::Ret(Typ::I8, i3));
        mir.print_block(&el_bb);
        function_translator.translate(el_bb);
    }

    println!();
    println!("== Cranelift ==");
    let func_id = function_translator.finish();
    translator.finish_function(func_id);
    let product = translator.finish();

    let objpath = NamedTempFile::with_suffix(".o").unwrap();
    fs::write(objpath.path(), product).unwrap();

    link(
        Path::new(&args.output),
        &[objpath.path()],
        args.target_triple,
    );
}
