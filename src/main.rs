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
    let my_var = mir.place("my_var");
    let temp_var = mir.place("temp_var");
    println!("== MIR ==");

    let loop_start_builder = mir.block();
    let loop_start = loop_start_builder.id();
    let loop_incr_builder = mir.block();
    let loop_incr = loop_incr_builder.id();
    let loop_incr2_builder = mir.block();
    let loop_incr2 = loop_incr2_builder.id();
    let end_builder = mir.block();
    let end = end_builder.id();

    {
        let i1 = mir.reg("i1");
        builder.comment("test");
        builder.emit(Inst::Imm(i1, Val::I8(0)));
        builder.emit(Inst::Alloca(my_var, Typ::I8));
        builder.emit(Inst::Store(my_var, Typ::I8, i1));
        let bb = builder.term(TermInst::Jmp(loop_start));
        mir.print_block(&bb);
        function_translator.translate(bb, true);
    }

    // loop_start
    {
        let mut builder = loop_start_builder;

        let imm10 = mir.reg("imm10");
        builder.emit(Inst::Imm(imm10, Val::I8(10)));
        let var_value = mir.reg("my_var_value");
        builder.emit(Inst::Load(var_value, Typ::I8, my_var));

        let cond = mir.reg("cond");
        builder.emit(Inst::Lt(cond, Typ::I8, var_value, imm10));

        let bb = builder.term(TermInst::If {
            cond,
            th: loop_incr,
            el: end,
        });
        mir.print_block(&bb);
        function_translator.translate(bb, false);
    }

    // loop_incr: temp_var := 1; goto loop_incr2;
    {
        let mut builder = loop_incr_builder;

        let imm1 = mir.reg("imm1");
        builder.emit(Inst::Imm(imm1, Val::I8(1)));

        builder.emit(Inst::Alloca(temp_var, Typ::I8));
        builder.emit(Inst::Store(temp_var, Typ::I8, imm1));

        let bb = builder.term(TermInst::Jmp(loop_incr2));
        mir.print_block(&bb);
        function_translator.translate(bb, false);
    }

    // loop_incr: my_var += temp_var; goto loop_start;
    {
        let mut builder = loop_incr2_builder;

        let my_var_value = mir.reg("my_var_value");
        builder.emit(Inst::Load(my_var_value, Typ::I8, my_var));

        let temp_var_value = mir.reg("temp_var_value");
        builder.emit(Inst::Load(temp_var_value, Typ::I8, temp_var));

        let sum = mir.reg("sum");
        builder.emit(Inst::Add(sum, Typ::I8, my_var_value, temp_var_value));

        builder.emit(Inst::Store(my_var, Typ::I8, sum));

        let bb = builder.term(TermInst::Jmp(loop_start));
        mir.print_block(&bb);
        function_translator.translate(bb, false);
    }

    function_translator.seal(loop_start);
    function_translator.seal(loop_incr);
    function_translator.seal(loop_incr2);

    {
        let mut builder = end_builder;
        let var_value = mir.reg("my_var_value");
        builder.emit(Inst::Load(var_value, Typ::I8, my_var));

        let bb = builder.term(TermInst::Ret(Typ::I8, var_value));
        mir.print_block(&bb);
        function_translator.translate(bb, true);
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
