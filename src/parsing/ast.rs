#![allow(unused)]

use std::{fmt, ops::Deref};

use internment::Intern;

use crate::errors::Span;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spanned<T> {
    pub t: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(t: T, span: Span) -> Self {
        Self { t, span }
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.t
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Typ {
    I64,
    Bool,
    Error,
}

impl fmt::Display for Typ {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Sum,
    Sub,
    Mul,
    Div,
    Rem,
    Lt,
    Le,
    Gt,
    Ge,
    Lor,
    Land,
    Eq,
    NEq,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = match self {
            BinOp::Sum => "+",
            BinOp::Sub => todo!(),
            BinOp::Mul => todo!(),
            BinOp::Div => todo!(),
            BinOp::Rem => todo!(),
            BinOp::Lt => todo!(),
            BinOp::Le => todo!(),
            BinOp::Gt => todo!(),
            BinOp::Ge => todo!(),
            BinOp::Lor => todo!(),
            BinOp::Land => todo!(),
            BinOp::Eq => todo!(),
            BinOp::NEq => todo!(),
        };
        f.write_str(x)
    }
}

#[derive(Debug)]
pub enum Unop {
    Neg,
    Lnot,
}

impl std::fmt::Display for Unop {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Ident(pub Intern<String>);

impl std::fmt::Display for Ident {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug)]
pub enum Expr {
    Int(i64, Span),
    Bool(bool, Span),
    BinOp {
        left: Box<Expr>,
        op: Spanned<BinOp>,
        right: Box<Expr>,
    },
    UnOp {
        op: Unop,
        operand: Box<Expr>,
    },
    Lval(Ident),
    Assignment {
        lhs: Ident,
        rhs: Box<Expr>,
    },
    Call {
        function: Box<Expr>,
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    VarDecl {
        lhs: Spanned<Ident>,
        typ: Option<(Typ, Span)>,
        rhs: Expr,
    },
    Expr(Expr),
    IfThenElse {
        cond: Expr,
        thbr: Box<Stmt>,
        elbr: Option<Box<Stmt>>,
    },
    Compound(Vec<Stmt>),
    Return(Expr),
}

pub trait TypCompatible {
    fn compatible(&self, other: Typ) -> bool;
}

impl TypCompatible for Typ {
    fn compatible(&self, other: Typ) -> bool {
        *self == other
    }
}

impl TypCompatible for Option<Typ> {
    fn compatible(&self, other: Typ) -> bool {
        if let Some(x) = self {
            x.compatible(other)
        } else {
            true
        }
    }
}
