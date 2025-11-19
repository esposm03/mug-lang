use chumsky::{
    prelude::*,
    text::{Char, ascii::ident},
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
    Dot,
    IntLit(u64),
    Ident(&'a str),
}

type Err<'a> = extra::Err<Rich<'a, char>>;

fn digit<'a>(radix: u32) -> impl Parser<'a, &'a str, char, Err<'a>> {
    any().filter(move |ch: &char| ch.is_digit(radix))
}

fn digits<'a>(radix: u32) -> impl Parser<'a, &'a str, String, Err<'a>> {
    digit(radix)
        .or(just('_').to('_'))
        .repeated()
        .at_least(1)
        .collect()
}

fn integer<'a>(radix: u32) -> impl Parser<'a, &'a str, Token<'a>, Err<'a>> {
    digits(radix)
        .and_is(digit(radix))
        .map(|s| s.replace("_", ""))
        .map(|s| s.parse().unwrap())
        .map(Token::IntLit)
}

fn token<'a>() -> impl Parser<'a, &'a str, Token<'a>, Err<'a>> {
    use Token::*;
    integer(10)
        .or(ident().map(Ident))
        .or(choice([
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
            just(".").to(Dot),
            just(";").to(Semicolon),
        ]))
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

#[test]
#[cfg(test)]
fn lex_integer_10() {
    assert!(integer(10).check("_").has_errors());
    assert!(integer(10).check("_0").has_errors());
    assert!(integer(10).check("_ 0").has_errors());
    assert_eq!(integer(10).parse("2_0").unwrap(), Token::IntLit(20));
    assert_eq!(integer(10).parse("2__0").unwrap(), Token::IntLit(20));
    assert_eq!(integer(10).parse("2_000").unwrap(), Token::IntLit(2_000));
}
