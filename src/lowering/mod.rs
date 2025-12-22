use std::mem;

use crate::{
    errors::{BinopTypeMismatchError, MugErr, Span, TypeMismatchError, UnknownVarError},
    mir::{
        BasicBlock, Inst, Place, Reg, TermInst, Val,
        build::{BbBuilder, MirBuilder},
    },
    parsing::ast::{BinOp, Expr, Ident, Spanned, Stmt, Typ, TypCompatible},
};

pub enum Event {
    Function(String),
    Block(BasicBlock),
    #[expect(unused)]
    DisableAutoSeal,
    #[expect(unused)]
    EnableAutoSeal,
}

struct VariableRegistry {
    vars: im::HashMap<Ident, Var>,
    stack: Vec<im::HashMap<Ident, Var>>,
}

impl VariableRegistry {
    fn new() -> Self {
        Self {
            vars: im::HashMap::new(),
            stack: vec![],
        }
    }

    fn get(&self, name: &Ident) -> Option<&Var> {
        self.vars.get(name)
    }

    fn set(&mut self, name: Spanned<Ident>, place: Place, typ: Typ) {
        self.vars.insert(
            name.t,
            Var {
                typ,
                place,
                declared_at: name.span,
            },
        );
    }

    #[expect(dead_code)]
    fn push_scope(&mut self) {
        self.stack.push(self.vars.clone());
    }

    #[expect(dead_code)]
    fn pop_scope(&mut self) {
        self.vars = self.stack.pop().expect("Empty variable stack");
    }
}

#[derive(Clone)]
struct Var {
    typ: Typ,
    place: Place,
    #[expect(dead_code)]
    declared_at: Span,
}

pub struct Lower {
    mir: MirBuilder,
    builder: BbBuilder,
    events: Vec<Event>,
    errors: Vec<MugErr>,

    vars: VariableRegistry,
}

impl Lower {
    pub fn new() -> Self {
        let mut mir = MirBuilder::new();
        let builder = mir.block();
        Lower {
            mir,
            builder,
            vars: VariableRegistry::new(),
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

    fn imm(&mut self, dest: Reg, v: Val) -> Typ {
        self.emit(Inst::Imm(dest, v));

        match v {
            Val::I8(_) => todo!(),
            Val::I64(_) => Typ::I64,
            Val::False => Typ::Bool,
            Val::True => Typ::Bool,
        }
    }

    fn get_var(&mut self, name: &Spanned<Ident>) -> Option<Var> {
        let v = self.vars.get(name);
        if v.is_none() {
            self.errors.push(Box::new(UnknownVarError(*name)))
        }
        v.cloned()
    }

    pub fn lower_expr(&mut self, expr: &Expr, dest: Reg) -> (Typ, Span) {
        match expr {
            Expr::Unit(loc) => (Typ::Unit, *loc),
            Expr::Int(i, loc) => (self.imm(dest, Val::I64(*i)), *loc),
            Expr::Bool(i, loc) => (
                self.imm(dest, if *i { Val::True } else { Val::False }),
                *loc,
            ),
            Expr::BinOp { left, op, right } => (self.lower_binop(dest, *op, left, right), op.span),
            Expr::UnOp { .. } => todo!(),
            Expr::VarDecl { lhs, typ, rhs } => {
                self.lower_var_decl(*lhs, *typ, rhs);
                (Typ::Unit, lhs.span)
            }
            Expr::Lval(ident) => {
                let v = self.vars.get(ident).unwrap();
                let res = (v.typ, ident.span);
                if v.typ.is_valid() {
                    self.emit(Inst::Load(dest, convert_ast_typ(v.typ), v.place));
                }
                res
            }
            Expr::Assignment { lhs, rhs, loc } => {
                let tmp = self.mir.temp();
                let (rhs_typ, rhs_span) = self.lower_expr(rhs, tmp);

                if let Some(v) = self.get_var(lhs) {
                    if !v.typ.compatible(rhs_typ) {
                        self.errors.push(Box::new(TypeMismatchError {
                            span_total: *loc,
                            span1: lhs.span,
                            typ1: v.typ,
                            span2: rhs_span,
                            typ2: rhs_typ,
                        }));
                    }

                    self.emit(Inst::Store(v.place, convert_ast_typ(rhs_typ), tmp));
                }

                (Typ::Unit, *loc)
            }
            Expr::Call { .. } => todo!(),
            Expr::Sequence(exprs) => {
                if exprs.len() >= 2 {
                    for expr in &exprs[..exprs.len() - 1] {
                        let temp = self.mir.temp();
                        self.lower_expr(expr, temp);
                    }
                }

                let last = &exprs[exprs.len() - 1];
                self.lower_expr(last, dest)
            }
        }
    }

    pub fn lower_binop(&mut self, dest: Reg, op: Spanned<BinOp>, left: &Expr, right: &Expr) -> Typ {
        let val1 = self.mir.temp();
        let val2 = self.mir.temp();
        let (typ1, span1) = self.lower_expr(left, val1);
        let (typ2, span2) = self.lower_expr(right, val2);

        if typ1 != typ2 && typ1.is_valid() && typ2.is_valid() {
            self.errors.push(Box::new(TypeMismatchError {
                span_total: op.span,
                span1,
                typ1,
                span2,
                typ2,
            }));
            return Typ::Error;
        }

        let (out_typ, expected_typ) = match op.t {
            BinOp::Sum
            | BinOp::Sub
            | BinOp::Mul
            | BinOp::Div
            | BinOp::Rem
            | BinOp::Lt
            | BinOp::Le
            | BinOp::Gt
            | BinOp::Ge => (Typ::I64, Typ::I64),
            BinOp::Lor | BinOp::Land => (Typ::Bool, Typ::Bool),
            BinOp::Eq => (Typ::Bool, typ1),
            BinOp::NEq => (Typ::Bool, typ1),
        };
        if expected_typ != typ1 && typ1.is_valid() {
            self.errors.push(Box::new(BinopTypeMismatchError {
                span_total: op.span,
                op: op.t,
                arg_types: typ1,
                expected: expected_typ,
            }));
        }

        let mirtyp = match out_typ {
            Typ::I64 => Typ::I64,
            Typ::Bool => Typ::Bool,
            Typ::Unit => Typ::Unit,
            Typ::Error => unreachable!(),
        };

        let constructor = match op.t {
            BinOp::Sum => Inst::Add,
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
        self.builder.emit(constructor(dest, mirtyp, val1, val2));

        out_typ
    }

    #[expect(unused)]
    pub fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
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
        let dest = self.mir.temp();
        let (_typ, _loc) = self.lower_expr(expr, dest);
        let block =
            mem::replace(&mut self.builder, self.mir.block()).term(TermInst::Ret(Typ::I64, dest));
        self.events.push(Event::Block(block));
    }

    fn lower_var_decl(&mut self, lhs: Spanned<Ident>, ty: Option<(Typ, Span)>, rhs: &Expr) {
        let rhs_expr = self.mir.temp();
        let (mut typ, _rhs_span) = self.lower_expr(rhs, rhs_expr);
        let place = self.mir.place(&lhs.t.0);

        use crate::parsing::ast::TypCompatible;
        if let Some((declared_type, _loc)) = ty {
            if !typ.compatible(declared_type) {
                // self.errors.push(Box::new(TypeMismatchError {
                //     span_total: todo!(),
                //     span1: todo!(),
                //     typ1: todo!(),
                //     span2: todo!(),
                //     typ2: todo!(),
                // }));
                todo!("Need to push a TypeMismatchError")
            }
            typ = declared_type;
        }

        self.vars.set(lhs, place, typ);
        if typ.is_valid() {
            self.emit(Inst::Alloca(place, convert_ast_typ(typ)));
            self.emit(Inst::Store(place, convert_ast_typ(typ), rhs_expr));
        }
    }
}

#[track_caller]
fn convert_ast_typ(t: Typ) -> Typ {
    match t {
        Typ::I64 => Typ::I64,
        Typ::Bool => Typ::Bool,
        Typ::Unit => Typ::Unit,
        Typ::Error => panic!("tried to convert ast::Typ::Error -> mir::Typ"),
    }
}
