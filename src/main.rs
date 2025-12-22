use std::{fs, path::Path, process, str::FromStr};

use ariadne::Source;
use chumsky::{input::Stream, prelude::*};
use internment::Intern;
use logos::Logos;
use target_lexicon::Triple;
use tempfile::NamedTempFile;

use crate::{
    backends::clif,
    errors::{MugErr, Span},
    linker::link,
    lowering::{Event, Lower},
    parsing::{lexer::Token, parser::expr_sequence},
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
    #[argh(positional, default = "String::from(\"input.mug\")")]
    input: String,
}

fn run(args: &Args) -> Result<(), Vec<MugErr>> {
    let src = fs::read_to_string(&args.input).map_err(|e| errors::reading_file(&args.input, e))?;
    let filename = Intern::from_ref(&args.input);

    let lexer = Token::lexer(&src).spanned().map(move |(tok, span)| {
        let tok = tok.unwrap_or(Token::Error);
        let span = Span::new(filename, span);
        (tok, span)
    });
    let str = Stream::from_iter(lexer).map(Span::new(filename, 0..src.len()), |ts| ts);

    let ast = expr_sequence()
        .parse(str)
        .into_result()
        .map_err(|e| e.into_iter().map(From::from).collect::<Vec<_>>())?;

    let mut lower = Lower::new();
    lower.lower_return(&ast);
    let mut events = lower.finish().unwrap_or_else(|errors| {
        for err in errors {
            err.report().eprint((filename, Source::from(&src))).unwrap();
        }
        process::exit(1);
    });
    events.insert(0, Event::Function("main".to_string())); // TODO: remove this

    let object = clif::handle_events(args, events);

    let objpath = NamedTempFile::with_suffix(".o").unwrap();
    fs::write(objpath.path(), object).unwrap();

    link(
        Path::new(&args.output),
        &[objpath.path()],
        &args.target_triple,
    );

    Ok(())
}

fn main() {
    let args = argh::from_env::<Args>();

    if let Err(errors) = run(&args) {
        let mut cache = MugCache::default();
        for e in errors {
            e.report().eprint(&mut cache).unwrap();
        }

        process::exit(1);
    }
}

#[derive(Default)]
struct MugCache(ariadne::FileCache);

impl ariadne::Cache<Intern<String>> for MugCache {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &Intern<String>,
    ) -> Result<&Source<Self::Storage>, impl std::fmt::Debug> {
        self.0.fetch(Path::new(id.as_str()))
    }

    fn display<'a>(&self, id: &'a Intern<String>) -> Option<impl std::fmt::Display + 'a> {
        self.0.display(Path::new(id.as_str()))
    }
}
