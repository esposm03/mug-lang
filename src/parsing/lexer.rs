use logos::{Lexer, Logos};

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(skip r"[ \t\n\f]+")]
pub enum Token<'a> {
    Error,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Rem,
    #[token("<")]
    Lt,
    #[token("<=")]
    Le,
    #[token(">")]
    Gt,
    #[token(">=")]
    Ge,
    #[token("||")]
    Lor,
    #[token("&&")]
    Land,
    #[token("!")]
    Lnot,
    #[token("==")]
    Eq,
    #[token("!=")]
    NEq,
    #[token("let")]
    Let,
    #[token("=")]
    Assign,
    #[token(";")]
    Semicolon,
    #[token(".")]
    Dot,
    #[token("return")]
    Return,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[regex(r"-?[0-9][0-9_]*", int_10)]
    IntLit(i64),
    #[regex(r"[[:alpha:]_][[:alnum:]]*")]
    Ident(&'a str),
}

impl std::fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Error => write!(f, "<error>"),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Mul => write!(f, "*"),
            Token::Div => write!(f, "/"),
            Token::Rem => write!(f, "%"),
            Token::Lt => write!(f, "<"),
            Token::Le => write!(f, "<="),
            Token::Gt => write!(f, ">"),
            Token::Ge => write!(f, ">="),
            Token::Lor => write!(f, "||"),
            Token::Land => write!(f, "&&"),
            Token::Lnot => write!(f, "!"),
            Token::Eq => write!(f, "=="),
            Token::NEq => write!(f, "!="),
            Token::Let => write!(f, "let"),
            Token::Assign => write!(f, "="),
            Token::Semicolon => write!(f, ";"),
            Token::Dot => write!(f, "."),
            Token::Return => write!(f, "return"),
            Token::IntLit(s) => write!(f, "int_lit({s})"),
            Token::Ident(s) => write!(f, "ident({s})"),
            Token::LParen => write!(f, "("),
            Token::RParen => write!(f, ")"),
            Token::Comma => write!(f, ","),
        }
    }
}

fn int_10<'a>(lex: &mut Lexer<'a, Token<'a>>) -> i64 {
    let slice = lex.slice().replace("_", "");
    i64::from_str_radix(&slice, 10).unwrap()
}

#[test]
#[cfg(test)]
fn lex_integers() {
    fn lex<'a>(s: &'a str) -> Vec<Result<Token<'a>, ()>> {
        Token::lexer(s).collect::<Vec<_>>()
    }

    assert_eq!(lex("_"), vec![Ok(Token::Ident("_"))]);
    assert_eq!(lex("_0"), vec![Ok(Token::Ident("_0"))]);
    assert_eq!(lex("2_0"), vec![Ok(Token::IntLit(20))]);
    assert_eq!(lex("2__0"), vec![Ok(Token::IntLit(20))]);

    assert_eq!(
        lex("_ 0"),
        vec![Ok(Token::Ident("_")), Ok(Token::IntLit(0))]
    );
}
