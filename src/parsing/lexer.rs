use chumsky::{
    prelude::*,
    text::{ascii::ident, int},
};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<'a> {
    Plus,
    Minus,
    Mul,
    Div,
    Rem,
    Lt,
    Le,
    Gt,
    Ge,
    Lor,
    Land,
    Lnot,
    Eq,
    NEq,
    Let,
    Assign,
    Semicolon,
    IntLit(u64),
    Ident(&'a str),
}

type Err<'a> = extra::Err<Rich<'a, char>>;

fn token<'a>() -> impl Parser<'a, &'a str, Token<'a>, Err<'a>> {
    use Token::*;
    choice([
        just("+").to(Plus),
        just("-").to(Minus),
        just("*").to(Mul),
        just("/").to(Div),
        just("%").to(Rem),
        just(">=").to(Ge),
        just(">").to(Gt),
        just("<=").to(Le),
        just("<").to(Lt),
        just("||").to(Lor),
        just("&&").to(Land),
        just("==").to(Eq),
        just("!=").to(NEq),
        just("!").to(Lnot),
        just("let").to(Let),
        just("=").to(Assign),
        just(";").to(Semicolon),
    ])
    .or(int(10).map(|s: &str| s.parse().unwrap()).map(IntLit))
    .or(ident().map(Ident))
    .padded()
}

pub fn tokens<'a>() -> impl Parser<'a, &'a str, Vec<Token<'a>>, Err<'a>> {
    token().repeated().collect::<Vec<_>>()
}

#[test]
#[ignore]
#[cfg(test)]
fn lex_operator_combination() {
    assert!(tokens().check("+==").has_errors());
    assert!(!tokens().check("+ ==").has_errors());
}
