use std::mem;

use crate::{
    errors::{BinopTypeMismatchError, MugErr, Span, TypeMismatchError},
    mir::{
        BasicBlock, Inst, Reg, TermInst, Typ, Val,
        build::{BbBuilder, MirBuilder},
    },
    parsing::ast::{BinOp, Expr, Lval, Stmt, Typ as AstTyp},
};

pub enum Event {
    Function(String),
    Block(BasicBlock),
    #[expect(unused)]
    DisableAutoSeal,
    #[expect(unused)]
    EnableAutoSeal,
}

pub struct Lower {
    mir: MirBuilder,
    builder: BbBuilder,
    events: Vec<Event>,
    errors: Vec<MugErr>,
}

impl Lower {
    pub fn new() -> Self {
        let mut mir = MirBuilder::new();
        let builder = mir.block();
        Lower {
            mir,
            builder,
            events: vec![],
            errors: vec![],
        }
    }

    pub fn finish(self) -> Result<Vec<Event>, Vec<MugErr>> {
        if self.errors.is_empty() {
            Ok(self.events)
        } else {
            Err(self.errors)
        }
    }

    fn emit(&mut self, inst: Inst) {
        self.builder.emit(inst);
    }

    fn imm(&mut self, dest: Reg, v: Val) -> AstTyp {
        self.emit(Inst::Imm(dest, v));

        match v {
            Val::I8(_) => todo!(),
            Val::I64(_) => AstTyp::Int,
            Val::False => AstTyp::Bool,
            Val::True => AstTyp::Bool,
        }
    }

    pub fn lower_expr(&mut self, expr: &Expr) -> (AstTyp, Span, Reg) {
        let dest = self.mir.temp();
        let (typ, loc) = match expr {
            Expr::Int(i, loc) => (self.imm(dest, Val::I64(*i)), loc),
            Expr::Bool(i, loc) => (self.imm(dest, if *i { Val::True } else { Val::False }), loc),
            Expr::BinOp { left, op, right } => {
                (self.lower_binop(dest, *op, left, right), &op.span())
            }
            #[expect(unused)]
            Expr::UnOp { op, operand } => todo!(),
            #[expect(unused)]
            Expr::Lval(lval) => todo!(),
            #[expect(unused)]
            Expr::Assignment { lhs, rhs } => todo!(),
            #[expect(unused)]
            Expr::Call { function, args } => todo!(),
        };
        (typ, *loc, dest)
    }

    pub fn lower_binop(&mut self, dest: Reg, op: BinOp, left: &Expr, right: &Expr) -> AstTyp {
        let (typ1, span1, val1) = self.lower_expr(left);
        let (typ2, span2, val2) = self.lower_expr(right);

        if typ1 != typ2 {
            self.errors.push(Box::new(TypeMismatchError {
                span_total: op.span(),
                span1,
                typ1,
                span2,
                typ2,
            }));
            return AstTyp::Error;
        }

        let (out_typ, expected_typ) = match op {
            BinOp::Sum(_)
            | BinOp::Sub(_)
            | BinOp::Mul(_)
            | BinOp::Div(_)
            | BinOp::Rem(_)
            | BinOp::Lt(_)
            | BinOp::Le(_)
            | BinOp::Gt(_)
            | BinOp::Ge(_) => (AstTyp::Int, AstTyp::Int),
            BinOp::Lor(_) | BinOp::Land(_) => (AstTyp::Bool, AstTyp::Bool),
            BinOp::Eq(_) => (AstTyp::Bool, typ1),
            BinOp::NEq(_) => (AstTyp::Bool, typ1),
        };
        if expected_typ != typ1 {
            self.errors.push(Box::new(BinopTypeMismatchError {
                span_total: op.span(),
                op,
                arg_types: typ1,
                expected: expected_typ,
            }));
        }

        let mirtyp = match out_typ {
            AstTyp::Int => Typ::I8,
            AstTyp::Bool => Typ::Bool,
            AstTyp::Error => unreachable!(),
        };

        let constructor = match op {
            BinOp::Sum(_) => Inst::Add,
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
        self.builder.emit(constructor(dest, mirtyp, val1, val2));

        out_typ
    }

    #[expect(unused)]
    pub fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::VarDecl { lhs, typ, rhs } => self.lower_var_decl(lhs, *typ, rhs),
            #[expect(unused)]
            Stmt::Expr(expr) => todo!(),
            #[expect(unused)]
            Stmt::IfThenElse { cond, thbr, elbr } => todo!(),
            #[expect(unused)]
            Stmt::Compound(stmts) => todo!(),
            Stmt::Return(expr) => self.lower_return(expr),
        }
    }

    pub fn lower_return(&mut self, expr: &Expr) {
        // TODO: check return type
        let (_typ, _loc, expr) = self.lower_expr(expr);
        let block =
            mem::replace(&mut self.builder, self.mir.block()).term(TermInst::Ret(Typ::I8, expr));
        self.events.push(Event::Block(block));
    }

    #[allow(unused)]
    fn lower_var_decl(&mut self, lhs: &Lval, ty: Option<(AstTyp, Span)>, rhs: &Expr) {
        let rhs_dest = self.lower_expr(rhs);
        let place = self.mir.place(&lhs.0);

        // if !ty.compatible() {}

        self.emit(Inst::Alloca(place, todo!()));

        // self.emit(Inst::Store(place, todo!(), rhs_dest));
    }
}
