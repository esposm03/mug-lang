use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::select;

use crate::errors::Span;
use crate::parsing::ast::BinOp;
use crate::parsing::ast::Expr;
use crate::parsing::ast::Lval;
use crate::parsing::lexer::Token;

// Utilities only needed because `impl Trait` in type aliases is _STILL_ unstable.
#[doc(hidden)]
pub trait ParseInput<'a>: ValueInput<'a, Token = Token<'a>, Span = Span> {}
impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = Span>> ParseInput<'a> for I {}
type ParseErr<'a> = extra::Err<Rich<'a, Token<'a>, Span>>;

pub fn expr<'a, I: ParseInput<'a>>() -> impl Parser<'a, I, Expr, ParseErr<'a>> {
    use Token::*;

    let ident = select! { Ident(x) => Expr::Lval(Lval(x.to_string())) };
    let int_lit = select! { IntLit(x) = e => (x, e.span()) };
    let negative_int = just(Minus)
        .ignore_then(int_lit)
        .map(|(x, loc)| Expr::Int(-x, loc));
    let positive_int = int_lit.map(|(x, loc)| Expr::Int(x, loc));
    let bool = choice([just(True).to(true), just(False).to(false)])
        .map_with(|b, e| Expr::Bool(b, e.span()));

    recursive(|expr| {
        let arglist = expr
            .clone()
            .separated_by(just(Comma))
            .allow_trailing()
            .collect::<Vec<_>>();
        let call = ident
            .then(arglist.delimited_by(just(LParen), just(RParen)))
            .map(|(name, args)| Expr::Call {
                function: Box::new(name),
                args,
            })
            .boxed();
        let atom = call.or(negative_int).or(positive_int).or(ident).or(bool);
        let parens = atom.or(expr.delimited_by(just(LParen), just(RParen)));

        let l1_binop = just(Mul)
            .map_with(|_, e| BinOp::Mul(e.span()))
            .or(just(Div).map_with(|_, e| BinOp::Div(e.span())))
            .or(just(Rem).map_with(|_, e| BinOp::Rem(e.span())));
        let l2_binop = just(Plus)
            .map_with(|_, e| BinOp::Sum(e.span()))
            .or(just(Minus).map_with(|_, e| BinOp::Sub(e.span())));

        let product = parens
            .clone()
            .foldl(l1_binop.then(parens.clone()).repeated(), make_binop);
        let sum = product
            .clone()
            .foldl(l2_binop.then(product).repeated(), make_binop);

        sum
    })
}

fn make_binop(a: Expr, (op, b): (BinOp, Expr)) -> Expr {
    Expr::BinOp {
        op,
        left: Box::new(a),
        right: Box::new(b),
    }
}
