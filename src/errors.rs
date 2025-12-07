use std::{fmt::Display, ops};

use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};
use chumsky::error::{Rich, RichReason};
use internment::Intern;

use crate::parsing::ast;

type SpanTy<'a> = (&'a str, ops::Range<usize>);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    source: Intern<String>,
    start: usize,
    end: usize,
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
    fn report(self: Box<Self>) -> ariadne::Report<'static, Span>;
}

pub type MugErr = Box<dyn MugError>;

pub struct TypeMismatchError {
    pub span_total: Span,
    pub span1: Span,
    pub typ1: ast::Typ,
    pub span2: Span,
    pub typ2: ast::Typ,
}

impl MugError for TypeMismatchError {
    fn report(self: Box<Self>) -> ariadne::Report<'static, Span> {
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
    fn report(self: Box<Self>) -> ariadne::Report<'static, Span> {
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

fn str_join_iterator<'a, D: Display, I: ExactSizeIterator<Item = D>>(it: I) -> String {
    use std::fmt::Write;
    let mut res = String::new();

    let len = it.len();
    for (i, exp) in it.enumerate() {
        write!(res, "{exp}").unwrap();

        if i + 2 <= len {
            res.push_str(", ");
        }
        if i + 2 == len {
            res.push_str("or ");
        }
    }

    res
}

pub fn reason_to_msg<T: Display>(s: &RichReason<T>) -> String {
    match s {
        RichReason::ExpectedFound { expected, .. } => {
            if expected.len() == 1 {
                format!("expected {}", expected[0])
            } else {
                format!("expected one of {}", str_join_iterator(expected.iter()))
            }
        }
        RichReason::Custom(s) => s.clone(),
    }
}

pub fn reason_to_label<'a, T: Clone + Display>(span: Span, reason: &RichReason<T>) -> Label<Span> {
    let col = ariadne::Color::Red;

    let msg = match reason {
        RichReason::ExpectedFound { found: Some(x), .. } => {
            let tick = "'".fg(col);
            format!("found {tick}{}{tick}", x.to_string().fg(col))
        }
        RichReason::ExpectedFound { found: None, .. } => {
            let eof = "EOF".fg(col);
            format!("found {eof}")
        }
        RichReason::Custom(s) => s.clone(),
    };

    Label::new(span).with_message(msg).with_color(col)
}

#[must_use]
pub fn report_error<'a, T: Clone + Display>(err: Rich<'a, T, Span>) -> Report<'a, Span> {
    let sp = err.span().clone();
    Report::build(ReportKind::Error, sp.clone())
        .with_message(reason_to_msg(err.reason()))
        .with_label(reason_to_label(sp, err.reason()))
        .finish()
}
