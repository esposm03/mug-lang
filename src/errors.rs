use std::{fmt::Display, io, ops};

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind};
use chumsky::{DefaultExpected, label::LabelError, util::MaybeRef};
use internment::Intern;

use crate::parsing::{
    ast,
    lexer::Token,
    parser::{MugParser, ParseInput},
};

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

pub struct MugIoError {
    name: String,
    err: io::Error,
}

impl MugError for MugIoError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        ariadne::Report::build(
            ReportKind::Error,
            Span {
                source: Intern::from_ref(&self.name),
                start: 0,
                end: 0,
            },
        )
        .with_message(format!("Failed to read file `{}`: {}", self.name, self.err))
        .finish()
    }
}

pub fn reading_file(filename: &str, err: io::Error) -> Vec<MugErr> {
    vec![Box::new(MugIoError {
        err,
        name: filename.to_string(),
    })]
}

pub struct TypeMismatchError {
    pub span_total: Span,
    pub span1: Span,
    pub typ1: ast::Typ,
    pub span2: Span,
    pub typ2: ast::Typ,
}

impl MugError for TypeMismatchError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        let mut colors = ColorGenerator::new();
        Report::build(ReportKind::Error, self.span_total)
            // .with_code(3)
            .with_message(format!("Incompatible types"))
            .with_label(
                Label::new(self.span1)
                    .with_message(format!("This is of type {}", self.typ1.fg(colors.next()))),
            )
            .with_label(
                Label::new(self.span2)
                    .with_message(format!("This is of type {}", self.typ2.fg(colors.next()))),
            )
            .finish()
    }
}

pub struct BinopTypeMismatchError {
    pub span_total: Span,
    pub op: ast::BinOp,
    pub arg_types: ast::Typ,
    pub expected: ast::Typ,
}

impl MugError for BinopTypeMismatchError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        let mut colors = ColorGenerator::new();
        let expected = self.expected.fg(colors.next());
        let found = self.arg_types.fg(colors.next());
        Report::build(ReportKind::Error, self.span_total)
            // .with_code(3)
            .with_message(format!(
                "Can't use the `{}` operator on arguments of type `{}`",
                self.op, found,
            ))
            .with_label(Label::new(self.span_total).with_message(format!(
                "No implementation for `{} {} {}`",
                found, self.op, found,
            )))
            .with_note(format!("Expected arguments to have type {}", expected))
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseExpected {
    Unknown,
    Identifier,
    IntLit,
    BoolLit,
    Expression,
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub expected: ParseExpected,
    pub found: Option<Token>,
    pub span: Span,
}

impl<'a, I: ParseInput<'a>> LabelError<'a, I, DefaultExpected<'a, Token>> for ParseError {
    fn expected_found<E: IntoIterator<Item = DefaultExpected<'a, Token>>>(
        _expected: E,
        found: Option<MaybeRef<'a, Token>>,
        span: Span,
    ) -> Self {
        Self {
            expected: ParseExpected::Unknown,
            found: found.map(|maybe| maybe.into_inner()),
            span,
        }
    }
}
impl<'a, I: ParseInput<'a>> chumsky::error::Error<'a, I> for ParseError {
    fn merge(mut self, other: Self) -> Self {
        let is_expr =
            |e: &ParseExpected| matches!(e, ParseExpected::Identifier | ParseExpected::IntLit);

        if self.expected == ParseExpected::Unknown {
            self.expected = other.expected
        } else if is_expr(&self.expected) && is_expr(&other.expected) {
            self.expected = ParseExpected::Expression;
        }

        self
    }
}

impl MugError for ParseError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        let expected = match self.expected {
            ParseExpected::Unknown => "<unknown>",
            ParseExpected::Identifier => "an identifier",
            ParseExpected::IntLit => "an integer",
            ParseExpected::BoolLit => "a boolean",
            ParseExpected::Expression => "an expression",
        };
        let found = self.found.unwrap_or(Token::Eof);
        let mut builder = Report::build(ReportKind::Error, self.span)
            .with_message(format!("expected {expected}, found `{}`", found))
            .with_label(
                Label::new(self.span)
                    .with_message(format!("Expected {expected}"))
                    .with_color(Color::Red),
            );

        if matches!(self.found, Some(Token::IntLit(_))) {
            builder = builder.with_note("identifiers cannot start with a number");
        }
        builder.finish()
    }
}

pub trait MugParserWanted<'a, I: ParseInput<'a>, O>: MugParser<'a, I, O> {
    fn wanted(self, expected: ParseExpected) -> impl MugParser<'a, I, O>;
}

impl<'a, I: ParseInput<'a>, O, P: MugParser<'a, I, O> + Clone> MugParserWanted<'a, I, O> for P {
    fn wanted(self, expected: ParseExpected) -> impl MugParser<'a, I, O> {
        self.map_err_with_state(move |prev, span, _ctx| ParseError {
            expected,
            found: prev.found,
            span,
        })
    }
}
