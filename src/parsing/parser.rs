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

pub trait MugParser<'a, I: ParseInput<'a>, O>: Parser<'a, I, O, ParseExtra> + Clone + 'a {
    fn spanned(self) -> impl MugParser<'a, I, crate::parsing::ast::Spanned<O>>;
}
impl<'a, I: ParseInput<'a>, O: 'a, P: Parser<'a, I, O, ParseExtra> + Clone + 'a> MugParser<'a, I, O>
    for P
{
    fn spanned(self) -> impl MugParser<'a, I, crate::parsing::ast::Spanned<O>> {
        self.map_with(|t, s| Spanned { t, span: s.span() })
    }
}

// ===== Combinators =====

#[allow(dead_code)]
fn is_parser<'a, O, I: ParseInput<'a>>(_: &impl Parser<'a, I, O, ParseExtra>) {}

fn comma_list<'a, I: ParseInput<'a>, O: 'a>(
    p: impl MugParser<'a, I, O>,
) -> impl MugParser<'a, I, Vec<O>> {
    p.separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn semicolon_list<'a, I: ParseInput<'a>, O: 'a>(
    p: impl MugParser<'a, I, O>,
) -> impl MugParser<'a, I, Vec<O>> {
    p.separated_by(just(Token::Semicolon).repeated().at_least(1))
        .allow_trailing()
        .collect::<Vec<_>>()
}

fn parens<'a, I: ParseInput<'a>, O: 'a>(p: impl MugParser<'a, I, O>) -> impl MugParser<'a, I, O> {
    p.delimited_by(just(Token::LRound), just(Token::RRound))
}

fn parens_comma_list<'a, I: ParseInput<'a>, O: 'a>(
    p: impl MugParser<'a, I, O>,
) -> impl MugParser<'a, I, Vec<O>> {
    parens(comma_list(p))
}

// ===== Atoms =====

fn unit_lit<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    just(Token::LRound)
        .then(just(Token::RRound))
        .map_with(|_, s| Expr::Unit(s.span()))
}

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

fn ident<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Ident> {
    select! { Token::Ident(x) => Ident(x) }.wanted(ParseExpected::Identifier)
}

fn call<'a, I: ParseInput<'a>>(expr: impl MugParser<'a, I, Expr>) -> impl MugParser<'a, I, Expr> {
    ident()
        .spanned()
        .then(parens_comma_list(expr.clone()))
        .map(|(name, args)| Expr::Call {
            function: Box::new(Expr::Lval(name)),
            args,
        })
}

fn assign<'a, I: ParseInput<'a>>(expr: impl MugParser<'a, I, Expr>) -> impl MugParser<'a, I, Expr> {
    ident()
        .spanned()
        .then_ignore(just(Token::Eq))
        .then(expr)
        .map_with(|(lhs, rhs), s| Expr::Assignment {
            lhs,
            rhs: Box::new(rhs),
            loc: s.span(),
        })
}

fn vardecl<'a, I: ParseInput<'a>>(
    expr: impl MugParser<'a, I, Expr>,
) -> impl MugParser<'a, I, Expr> {
    just(Token::Let)
        .ignore_then(ident().spanned())
        .then_ignore(just(Token::Eq))
        .then(expr)
        .map(|(name, expr)| Expr::VarDecl {
            lhs: name,
            typ: None,
            rhs: Box::new(expr),
        })
}

fn if_else<'a, I: ParseInput<'a>>(
    expr: impl MugParser<'a, I, Expr>,
) -> impl MugParser<'a, I, Expr> {
    let body = just(Token::LCurly)
        .ignore_then(expr.clone().or_not().spanned())
        .then_ignore(just(Token::RCurly))
        // If the body is empty, parse it as a unit constructor
        .map(|e| e.t.unwrap_or(Expr::Unit(e.span)));

    let pr_if = just(Token::If).ignore_then(expr.clone()).then(body.clone());
    let pr_else = just(Token::Else).ignore_then(body.clone());
    let pr_elif = recursive(|z| {
        just(Token::Elif)
            .ignore_then(expr.clone())
            .then(body.clone())
            .then(z.or_not())
            .map_with(|((cond, thbr), elbr), s| Expr::IfThenElse {
                cond: Box::new(cond),
                thbr: Box::new(thbr),
                elbr: elbr.map(Box::new),
                span: s.span(),
            })
    });

    pr_if
        .then(pr_else.or(pr_elif).map(Box::new).or_not())
        .map_with(|((cond, thbr), elbr), s| Expr::IfThenElse {
            cond: Box::new(cond),
            thbr: Box::new(thbr),
            span: s.span(),
            elbr,
        })
}

fn atom<'a, I: ParseInput<'a>>(expr: impl MugParser<'a, I, Expr>) -> impl MugParser<'a, I, Expr> {
    let atom = vardecl(expr.clone())
        .or(if_else(expr.clone()))
        .or(assign(expr.clone()))
        .or(call(expr))
        .or(negative_int())
        .or(positive_int())
        .or(ident().spanned().map(Expr::Lval))
        .or(bool())
        .or(unit_lit());

    parens(atom.clone()).or(atom)
}

pub fn expr<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    use Token::*;

    let make_binop = |a, (op, b)| Expr::BinOp {
        op,
        left: Box::new(a),
        right: Box::new(b),
    };

    recursive(|expr| {
        let l1_binop = choice((
            binop(Star, BinOp::Mul),
            binop(Slash, BinOp::Div),
            binop(Percent, BinOp::Rem),
        ));
        let l2_binop = choice([binop(Plus, BinOp::Sum), binop(Minus, BinOp::Sub)]);

        let product = atom(expr.clone()).foldl(l1_binop.then(atom(expr)).repeated(), make_binop);
        let sum = product
            .clone()
            .foldl(l2_binop.then(product).repeated(), make_binop);

        #[allow(clippy::let_and_return)]
        sum
    })
}

pub fn expr_sequence<'a, I: ParseInput<'a>>() -> impl MugParser<'a, I, Expr> {
    semicolon_list(expr()).map(Expr::Sequence)
}

#[cfg(test)]
mod testutils {
    use internment::Intern;

    use crate::{errors::Span, parsing::ast::Ident};

    macro_rules! parse_err {
        ($parser:expr, $src:expr, $expected:expr, $found:expr) => {
            let errors = $parser.parse(lex_str($src)).into_errors();
            assert_eq!(errors.len(), 1);

            assert_eq!(
                errors[0],
                ParseError {
                    expected: $expected,
                    found: Some($found),
                    span: testutils::span(0, $src.len()),
                }
            );
        };
    }
    pub(super) use parse_err;

    macro_rules! parse_ok {
        ($parser:expr, $src:expr, $expected:expr) => {
            assert_eq!($parser.parse(lex_str($src)).unwrap(), $expected)
        };
    }
    pub(super) use parse_ok;

    pub fn ident(s: &str) -> Ident {
        Ident(Intern::from_ref(s))
    }

    pub fn span(start: usize, end: usize) -> Span {
        let source = internment::Intern::from_ref("test.mug");
        Span { source, start, end }
    }
}

#[cfg(test)]
mod tests {
    use super::testutils::{parse_err, parse_ok};
    use super::*;
    use crate::{errors::ParseExpected::*, parsing::lexer::lex_str};

    #[test]
    fn test_ident() {
        parse_err!(ident(), "123", Identifier, Token::IntLit(123));
        parse_ok!(ident(), "_123", testutils::ident("_123"));
        parse_ok!(ident(), "abc", testutils::ident("abc"));
    }

    #[test]
    #[rustfmt::skip]
    fn test_expr_sequence() {
        parse_ok!(expr_sequence(), "1; 2", Expr::Sequence(vec![
            Expr::Int(1, testutils::span(0, 1)),
            Expr::Int(2, testutils::span(3, 4)),
        ]));
        parse_ok!(expr_sequence(), "1; 2;", Expr::Sequence(vec![
            Expr::Int(1, testutils::span(0, 1)),
            Expr::Int(2, testutils::span(3, 4)),
        ]));
        parse_ok!(expr_sequence(), "1;; 2;", Expr::Sequence(vec![
            Expr::Int(1, testutils::span(0, 1)),
            Expr::Int(2, testutils::span(4, 5)),
        ]));
    }
}
