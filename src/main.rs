use std::{fs, path::Path, process, str::FromStr};

use ariadne::Source;
use chumsky::{input::Stream, prelude::*};
use display_tree::{CharSet, Style, StyleBuilder, println_tree};
use logos::Logos;
use target_lexicon::Triple;
use tempfile::NamedTempFile;

use crate::{
    backends::clif,
    errors::report_error,
    linker::link,
    lowering::{Event, Lower},
    parsing::{lexer::Token, parser::expr},
};

mod backends;
mod errors;
mod linker;
mod lowering;
mod mir;
mod parsing;

fn parse_triple(s: &str) -> Result<Triple, String> {
    Triple::from_str(s).map_err(|e| format!("{e}"))
}

/// Compiler for the Dolphin language
#[derive(argh::FromArgs)]
pub struct Args {
    /// the target triple to compile for
    #[argh(option, default = "Triple::host()", from_str_fn(parse_triple))]
    target_triple: Triple,
    /// where to write the output to
    #[argh(option, short = 'o', default = "String::from(\"output\")")]
    output: String,
}

fn run<'a>(src: &'a str) -> Result<(), Vec<Rich<'a, Token<'a>>>> {
    let lexer = Token::lexer(src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, SimpleSpan::from(span)),
        Err(()) => (Token::Error, span.into()),
    });
    let str = Stream::from_iter(lexer).map((0..src.len()).into(), |ts| ts);

    let ast = expr().parse(str).into_result()?;
    println_tree!(
        ast,
        Style::default()
            .indentation(1)
            .char_set(CharSet::DOUBLE_LINE)
    );

    let mut lower = Lower::new();
    lower.lower_return(&ast);
    let mut events = lower.finish();
    events.insert(0, Event::Function("main".to_string())); // TODO: remove this

    let args = argh::from_env::<Args>();
    let object = clif::handle_events(&args, events);

    let objpath = NamedTempFile::with_suffix(".o").unwrap();
    fs::write(objpath.path(), object).unwrap();

    link(
        Path::new(&args.output),
        &[objpath.path()],
        args.target_triple,
    );

    Ok(())
}

fn main() {
    let src = "1+2+3";

    if let Err(errors) = run(src) {
        for e in errors {
            report_error("main.mug", e)
                .eprint(("main.mug", Source::from(src)))
                .unwrap();
        }

        process::exit(1);
    }

    // println!();
    // println!("== Cranelift ==");
    // let func_id = function_translator.finish();
    // translator.finish_function(func_id);
    // let product = translator.finish();

    // let objpath = NamedTempFile::with_suffix(".o").unwrap();
    // fs::write(objpath.path(), product).unwrap();

    // link(
    //     Path::new(&args.output),
    //     &[objpath.path()],
    //     args.target_triple,
    // );
}
