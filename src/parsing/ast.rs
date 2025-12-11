#![allow(unused)]

use std::fmt;

use display_tree::DisplayTree;
use internment::Intern;

use crate::errors::Span;

#[derive(Clone, Copy, Debug, DisplayTree, PartialEq, Eq)]
pub enum Typ {
    Int,
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
    Sum(Span),
    Sub(Span),
    Mul(Span),
    Div(Span),
    Rem(Span),
    Lt(Span),
    Le(Span),
    Gt(Span),
    Ge(Span),
    Lor(Span),
    Land(Span),
    Eq(Span),
    NEq(Span),
}

impl BinOp {
    pub fn span(&self) -> Span {
        match self {
            BinOp::Sum(span)
            | BinOp::Sub(span)
            | BinOp::Mul(span)
            | BinOp::Div(span)
            | BinOp::Rem(span)
            | BinOp::Lt(span)
            | BinOp::Le(span)
            | BinOp::Gt(span)
            | BinOp::Ge(span)
            | BinOp::Lor(span)
            | BinOp::Land(span)
            | BinOp::Eq(span)
            | BinOp::NEq(span) => *span,
        }
    }
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let x = match self {
            BinOp::Sum(_) => "+",
            BinOp::Sub(_) => todo!(),
            BinOp::Mul(_) => todo!(),
            BinOp::Div(_) => todo!(),
            BinOp::Rem(_) => todo!(),
            BinOp::Lt(_) => todo!(),
            BinOp::Le(_) => todo!(),
            BinOp::Gt(_) => todo!(),
            BinOp::Ge(_) => todo!(),
            BinOp::Lor(_) => todo!(),
            BinOp::Land(_) => todo!(),
            BinOp::Eq(_) => todo!(),
            BinOp::NEq(_) => todo!(),
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

#[derive(Debug)]
pub struct Lval(pub Intern<String>);

impl std::fmt::Display for Lval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug, DisplayTree)]
pub enum Expr {
    Int(#[node_label] i64, #[ignore] Span),
    Bool(bool, #[ignore] Span),
    BinOp {
        #[tree]
        left: Box<Expr>,
        #[field_label]
        op: BinOp,
        #[tree]
        right: Box<Expr>,
    },
    UnOp {
        op: Unop,
        #[tree]
        operand: Box<Expr>,
    },
    Lval(Lval),
    Assignment {
        #[field_label]
        lhs: Lval,
        #[tree]
        rhs: Box<Expr>,
    },
    Call {
        #[tree]
        function: Box<Expr>,
        #[ignore_field]
        args: Vec<Expr>,
    },
}

#[derive(Debug)]
pub enum Stmt {
    VarDecl {
        lhs: Lval,
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
