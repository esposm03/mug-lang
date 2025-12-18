use chumsky::{input::ValueInput, prelude::*, select};

use crate::{
    errors::{MugParserWanted, ParseError, ParseExpected, Span},
    parsing::{
        ast::{BinOp, Expr, Ident, Spanned},
        lexer::Token,
    },
};

/// A utility trait only needed because `impl Trait` in type aliases is _STILL_ unstable.
pub trait ParseInput<'a>: ValueInput<'a, Token = Token, Span = Span> {}
impl<'a, I: ValueInput<'a, Token = Token, Span = Span>> ParseInput<'a> for I {}
/// "Settings" for the parsers.
type ParseExtra = extra::Err<ParseError>;

pub trait MugParser<'a, I: ParseInput<'a>, O>: Parser<'a, I, O, ParseExtra> + Clone {}
impl<'a, I: ParseInput<'a>, O, P: Parser<'a, I, O, ParseExtra> + Clone> MugParser<'a, I, O> for P {}

// ===== Combinators =====

#[allow(dead_code)]
fn is_parser<'a, O, I: ParseInput<'a>>(_: &impl Parser<'a, I, O, ParseExtra>) {}

fn comma_list<'a, I: ParseInput<'a>, O>(
    p: impl MugParser<'a, I, O>,
) -> impl MugParser<'a, I, Vec<O>> {
    p.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn parens<'a, I: ParseInput<'a>, O>(p: impl MugParser<'a, I, O>) -> impl MugParser<'a, I, O> {
    p.delimited_by(just(Token::LParen), just(Token::RParen))
}

fn parens_comma_list<'a, I: ParseInput<'a>, O>(
    p: impl MugParser<'a, I, O>,
) -> impl MugParser<'a, I, Vec<O>> {
    parens(comma_list(p))
}

// ===== Atoms =====

fn int_lit<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, (i64, Span)> {
    select! { Token::IntLit(x) = e => (x, e.span()) }.wanted(ParseExpected::IntLit)
}

fn positive_int<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    int_lit().map(|(x, loc)| Expr::Int(x, loc))
}

fn negative_int<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    just(Token::Minus)
        .ignore_then(int_lit())
        .map(|(x, loc)| Expr::Int(-x, loc))
}

fn bool<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    choice([just(Token::True).to(true), just(Token::False).to(false)])
        .map_with(|b, e| Expr::Bool(b, e.span()))
        .wanted(ParseExpected::BoolLit)
}

fn binop<'a, I: ParseInput<'a>>(tok: Token, op: BinOp) -> impl MugParser<'a, I, Spanned<BinOp>> {
    just(tok)
        .to(op)
        .map_with(|op, e| Spanned::new(op, e.span()))
}

fn ident<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    select! { Token::Ident(x) => Expr::Lval(Ident(x)) }.wanted(ParseExpected::Identifier)
}

fn call<'a, I: ParseInput<'a>>(expr: impl MugParser<'a, I, Expr>) -> impl MugParser<'a, I, Expr> {
    ident()
        .then(parens_comma_list(expr.clone()))
        .map(|(name, args)| Expr::Call {
            function: Box::new(name),
            args,
        })
}

fn atom<'a, I: ParseInput<'a>>(expr: impl MugParser<'a, I, Expr>) -> impl MugParser<'a, I, Expr> {
    let atom = call(expr)
        .or(negative_int())
        .or(positive_int())
        .or(ident())
        .or(bool());

    parens(atom.clone()).or(atom)
}

pub fn expr<'a, I: ParseInput<'a>>() -> impl Parser<'a, I, Expr, ParseExtra> {
    use Token::*;

    recursive(|expr| {
        let l1_binop = choice((
            binop(Mul, BinOp::Mul),
            binop(Div, BinOp::Div),
            binop(Rem, BinOp::Rem),
        ));
        let l2_binop = choice([binop(Plus, BinOp::Sum), binop(Minus, BinOp::Sub)]);

        let product = atom(expr.clone()).foldl(l1_binop.then(atom(expr)).repeated(), make_binop);
        let sum = product
            .clone()
            .foldl(l2_binop.then(product).repeated(), make_binop);

        sum
    })
}

fn make_binop(a: Expr, (op, b): (Spanned<BinOp>, Expr)) -> Expr {
    Expr::BinOp {
        op,
        left: Box::new(a),
        right: Box::new(b),
    }
}

#[test]
#[cfg(test)]
fn test_ident() {
    use crate::{errors::ParseExpected::*, parsing::lexer::lex_str};
    use internment::Intern;

    macro_rules! case {
        ($parser:expr, $src:expr, $expected:expr, $found:expr) => {
            let errors = $parser.parse(lex_str($src)).into_errors();
            assert_eq!(errors.len(), 1);

            assert_eq!(
                errors[0],
                ParseError {
                    expected: $expected,
                    found: Some($found),
                    span: Span {
                        source: Intern::from_ref("test.mug"),
                        start: 0,
                        end: $src.len(),
                    },
                }
            );
        };
    }

    case!(ident(), "123", Identifier, Token::IntLit(123));
    case!(bool(), "123", BoolLit, Token::IntLit(123));
}
