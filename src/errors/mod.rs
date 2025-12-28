use std::{
    fmt::{Debug, Display},
    ops,
};

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

#[derive(Debug, PartialEq)]
pub enum MugErr {
    Io(MugIoError),
    Parse(ParseError),
    UnknownVar(UnknownVarError),
    TypeMismatch(TypeMismatchError),
    BinopTypeMismatch(BinopTypeMismatchError),
    ConditionNotBool(ConditionNotBoolError),
}

impl MugErr {
    pub fn report(self) -> Report<'static, Span> {
        match self {
            MugErr::Io(e) => e.report(),
            MugErr::Parse(e) => e.report(),
            MugErr::UnknownVar(e) => e.report(),
            MugErr::TypeMismatch(e) => e.report(),
            MugErr::BinopTypeMismatch(e) => e.report(),
            MugErr::ConditionNotBool(e) => e.report(),
        }
    }
}

impl From<MugIoError> for MugErr {
    fn from(value: MugIoError) -> Self {
        MugErr::Io(value)
    }
}
impl From<ParseError> for MugErr {
    fn from(value: ParseError) -> Self {
        MugErr::Parse(value)
    }
}
impl From<UnknownVarError> for MugErr {
    fn from(value: UnknownVarError) -> Self {
        MugErr::UnknownVar(value)
    }
}
impl From<TypeMismatchError> for MugErr {
    fn from(value: TypeMismatchError) -> Self {
        MugErr::TypeMismatch(value)
    }
}
impl From<BinopTypeMismatchError> for MugErr {
    fn from(value: BinopTypeMismatchError) -> Self {
        MugErr::BinopTypeMismatch(value)
    }
}
impl From<ConditionNotBoolError> for MugErr {
    fn from(value: ConditionNotBoolError) -> Self {
        MugErr::ConditionNotBool(value)
    }
}
