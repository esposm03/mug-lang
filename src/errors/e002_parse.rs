use ariadne::{Color, Label, Report, ReportKind};
use chumsky::{DefaultExpected, label::LabelError, util::MaybeRef};

use crate::{
    errors::Span,
    parsing::{
        lexer::Token,
        parser::{MugParser, ParseInput},
    },
};

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

impl ParseError {
    pub fn report(self) -> Report<'static, Span> {
        let expected = match self.expected {
            ParseExpected::Unknown => "<unknown>",
            ParseExpected::Identifier => "an identifier",
            ParseExpected::IntLit => "an integer",
            ParseExpected::BoolLit => "a boolean",
            ParseExpected::Expression => "an expression",
        };
        let found = self.found.unwrap_or(Token::Eof);
        let mut builder = Report::build(ReportKind::Error, self.span)
            .with_code(2)
            .with_message(format!("expected {expected}, found `{}`", found))
            .with_label(
                Label::new(self.span)
                    .with_message(format!("Expected {expected}"))
                    .with_color(Color::Red),
            );

        if matches!(self.found, Some(Token::IntLit(_)))
            && self.expected == ParseExpected::Identifier
        {
            builder = builder.with_note("identifiers cannot start with a number");
        }
        builder.finish()
    }
}

pub trait MugParserWanted<'a, I: ParseInput<'a>, O>: MugParser<'a, I, O> {
    fn wanted(self, expected: ParseExpected) -> impl MugParser<'a, I, O>;
}

impl<'a, I: ParseInput<'a>, O: 'a, P: MugParser<'a, I, O> + Clone> MugParserWanted<'a, I, O> for P {
    fn wanted(self, expected: ParseExpected) -> impl MugParser<'a, I, O> {
        self.map_err_with_state(move |prev, span, _ctx| ParseError {
            expected,
            found: prev.found,
            span,
        })
    }
}
