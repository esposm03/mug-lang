use std::{fmt::Display, ops};

use ariadne::Report;
use internment::Intern;

pub use e001_io::*;
pub use e002_parse::*;
pub use e003_unknown_var::*;
pub use e004_type_mismatch::*;
pub use e005_binop_type_mismatch::*;
pub use e006_if_cond_type::*;

mod e001_io;
mod e002_parse;
mod e003_unknown_var;
mod e004_type_mismatch;
mod e005_binop_type_mismatch;
mod e006_if_cond_type;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub source: Intern<String>,
    pub start: usize,
    pub end: usize,
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.source, self.start, self.end)
    }
}

impl ariadne::Span for Span {
    type SourceId = Intern<String>;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.start
    }

    fn end(&self) -> usize {
        self.end
    }
}

impl chumsky::span::Span for Span {
    type Context = Intern<String>;

    type Offset = usize;

    fn new(context: Self::Context, range: ops::Range<Self::Offset>) -> Self {
        Self {
            source: context,
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        self.source
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

pub trait MugError {
    fn report(self: Box<Self>) -> Report<'static, Span>;
}

pub type MugErr = Box<dyn MugError>;

impl<T: MugError + 'static> From<T> for MugErr {
    fn from(value: T) -> Self {
        Box::new(value)
    }
}
