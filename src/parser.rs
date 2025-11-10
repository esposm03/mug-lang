#[derive(Debug)]
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
#[derive(Debug)]
pub enum Unop {
    Neg,
    Lnot,
}

#[derive(Debug)]
pub struct Lval(pub String);

#[derive(Debug)]
pub enum Expr {
    Int(i64),
    Bool(bool),
    BinOp {
        left: Box<Expr>,
        op: BinOp,
        right: Box<Expr>,
    },
    UnOp {
        op: Unop,
        operand: Box<Expr>,
    },
    Lval(Lval),
    Assignment {
        lhs: Lval,
        rhs: Box<Expr>,
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
