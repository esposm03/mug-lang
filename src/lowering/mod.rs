use std::mem;

use crate::{
    mir::{
        BasicBlock, Inst, Reg, TermInst, Typ, Val,
        build::{BbBuilder, MirBuilder},
    },
    parsing::ast::{BinOp, Expr},
};

pub enum Event {
    Function(String),
    Block(BasicBlock),
    DisableAutoSeal,
    EnableAutoSeal,
}

pub struct Lower {
    mir: MirBuilder,
    builder: BbBuilder,
    events: Vec<Event>,
}

impl Lower {
    pub fn new() -> Self {
        let mut mir = MirBuilder::new();
        let builder = mir.block();
        Lower {
            mir,
            builder,
            events: vec![],
        }
    }

    pub fn finish(self) -> Vec<Event> {
        self.events
    }

    fn emit(&mut self, inst: Inst) {
        self.builder.emit(inst);
    }

    pub fn lower_expr(&mut self, expr: &Expr) -> Reg {
        let dest = self.mir.temp();
        match expr {
            Expr::Int(i) => self.emit(Inst::Imm(dest, Val::I64(*i))),
            Expr::Bool(i) => self.emit(Inst::Imm(dest, if *i { Val::True } else { Val::False })),
            Expr::BinOp { left, op, right } => self.lower_binop(dest, *op, left, right),
            #[expect(unused)]
            Expr::UnOp { op, operand } => todo!(),
            #[expect(unused)]
            Expr::Lval(lval) => todo!(),
            #[expect(unused)]
            Expr::Assignment { lhs, rhs } => todo!(),
            #[expect(unused)]
            Expr::Call { function, args } => todo!(),
        };
        dest
    }

    pub fn lower_binop(&mut self, dest: Reg, op: BinOp, left: &Expr, right: &Expr) {
        let constructor = match op {
            BinOp::Plus => Inst::Add,
            BinOp::Minus => todo!(),
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
        let left = self.lower_expr(left);
        let right = self.lower_expr(right);
        self.builder.emit(constructor(dest, Typ::I8, left, right));
    }

    pub fn lower_return(&mut self, expr: &Expr) {
        let expr = self.lower_expr(expr);
        let block =
            mem::replace(&mut self.builder, self.mir.block()).term(TermInst::Ret(Typ::I8, expr));
        self.events.push(Event::Block(block));
    }
}
