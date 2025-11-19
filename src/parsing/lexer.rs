#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token {
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
    IntLit(u64),
}

fn token<'a>() -> impl Parser<'a, &'a str, Token, extra::Err<Rich<'a, char>>> {
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
    ])
    .or(int(10).map(|s: &str| s.parse().unwrap()).map(IntLit))
    .padded()
}
