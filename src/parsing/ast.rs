#![allow(unused)]

use display_tree::DisplayTree;

#[derive(Debug, DisplayTree)]
pub enum Typ {
    Int,
    Bool,
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
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
    Eq,
    NEq,
}

impl std::fmt::Display for BinOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
pub struct Lval(pub String);

impl std::fmt::Display for Lval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.0)
    }
}

#[derive(Debug, DisplayTree)]
pub enum Expr {
    Int(#[node_label] i64),
    Bool(bool),
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
