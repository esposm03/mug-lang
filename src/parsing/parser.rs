use chumsky::input::ValueInput;
use chumsky::prelude::*;
use chumsky::select;

use crate::parsing::ast::BinOp;
use crate::parsing::ast::Expr;
use crate::parsing::ast::Lval;
use crate::parsing::lexer::Token;

// Utilities only needed because `impl Trait` in type aliases is _STILL_ unstable.
#[doc(hidden)]
pub trait ParseInput<'a>: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan> {}
impl<'a, I: ValueInput<'a, Token = Token<'a>, Span = SimpleSpan>> ParseInput<'a> for I {}
type ParseErr<'a> = extra::Err<Rich<'a, Token<'a>>>;

pub fn expr<'a, I: ParseInput<'a>>() -> impl Parser<'a, I, Expr, ParseErr<'a>> {
    use Token::*;

    let ident = select! { Ident(x) => Expr::Lval(Lval(x.to_string())) };
    let int_lit = select! { IntLit(x) => x };
    let negative_int = just(Minus).ignore_then(int_lit).map(|x| Expr::Int(-x));
    let positive_int = int_lit.map(|x| Expr::Int(x));

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
        let atom = call.or(negative_int).or(positive_int).or(ident);
        let parens = atom.or(expr.delimited_by(just(LParen), just(RParen)));

        let l1_binop = choice([
            just(Mul).to(BinOp::Mul),
            just(Div).to(BinOp::Div),
            just(Rem).to(BinOp::Rem),
        ]);
        let l2_binop = choice([just(Plus).to(BinOp::Plus), just(Minus).to(BinOp::Minus)]);

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
        left: Box::new(a),
        op,
        right: Box::new(b),
    }
}
