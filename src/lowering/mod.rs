use std::mem;

use crate::{
    errors::{BinopTypeMismatchError, MugErr, Span, TypeMismatchError},
    mir::{
        BasicBlock, Inst, Place, Reg, TermInst, Typ, Val,
        build::{BbBuilder, MirBuilder},
    },
    parsing::ast::{BinOp, Expr, Ident, Spanned, Stmt, Typ as AstTyp},
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

    fn set(&mut self, name: Spanned<Ident>, place: Place, typ: AstTyp) {
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
    #[expect(dead_code)]
    typ: AstTyp,
    #[expect(dead_code)]
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

    fn imm(&mut self, dest: Reg, v: Val) -> AstTyp {
        self.emit(Inst::Imm(dest, v));

        match v {
            Val::I8(_) => todo!(),
            Val::I64(_) => AstTyp::I64,
            Val::False => AstTyp::Bool,
            Val::True => AstTyp::Bool,
        }
    }

    pub fn lower_expr(&mut self, expr: &Expr, dest: Reg) -> (AstTyp, Span) {
        match expr {
            Expr::Int(i, loc) => (self.imm(dest, Val::I64(*i)), *loc),
            Expr::Bool(i, loc) => (
                self.imm(dest, if *i { Val::True } else { Val::False }),
                *loc,
            ),
            Expr::BinOp { left, op, right } => (self.lower_binop(dest, *op, left, right), op.span),
            #[expect(unused)]
            Expr::UnOp { op, operand } => todo!(),
            Expr::VarDecl { lhs, typ, rhs } => {
                self.lower_var_decl(*lhs, *typ, rhs);
                (AstTyp::Bool, lhs.span)
            }
            #[expect(unused)]
            Expr::Lval(ident) => {
                if let Some(v) = self.vars.get(ident) {}

                todo!()
            }
            #[expect(unused)]
            Expr::Assignment { lhs, rhs } => todo!(),
            #[expect(unused)]
            Expr::Call { function, args } => todo!(),
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

    pub fn lower_binop(
        &mut self,
        dest: Reg,
        op: Spanned<BinOp>,
        left: &Expr,
        right: &Expr,
    ) -> AstTyp {
        let val1 = self.mir.temp();
        let val2 = self.mir.temp();
        let (typ1, span1) = self.lower_expr(left, val1);
        let (typ2, span2) = self.lower_expr(right, val2);

        if typ1 != typ2 {
            self.errors.push(Box::new(TypeMismatchError {
                span_total: op.span,
                span1,
                typ1,
                span2,
                typ2,
            }));
            return AstTyp::Error;
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
            | BinOp::Ge => (AstTyp::I64, AstTyp::I64),
            BinOp::Lor | BinOp::Land => (AstTyp::Bool, AstTyp::Bool),
            BinOp::Eq => (AstTyp::Bool, typ1),
            BinOp::NEq => (AstTyp::Bool, typ1),
        };
        if expected_typ != typ1 {
            self.errors.push(Box::new(BinopTypeMismatchError {
                span_total: op.span,
                op: op.t,
                arg_types: typ1,
                expected: expected_typ,
            }));
        }

        let mirtyp = match out_typ {
            AstTyp::I64 => Typ::I8,
            AstTyp::Bool => Typ::Bool,
            AstTyp::Error => unreachable!(),
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
            mem::replace(&mut self.builder, self.mir.block()).term(TermInst::Ret(Typ::I8, dest));
        self.events.push(Event::Block(block));
    }

    #[allow(unused)]
    fn lower_var_decl(&mut self, lhs: Spanned<Ident>, ty: Option<(AstTyp, Span)>, rhs: &Expr) {
        let rhs_expr = self.mir.temp();
        let (mut typ, rhs_span) = self.lower_expr(rhs, rhs_expr);
        let place = self.mir.place(&lhs.t.0);

        use crate::parsing::ast::TypCompatible;
        if let Some((declared_type, loc)) = ty {
            if !typ.compatible(declared_type) {
                self.errors.push(Box::new(TypeMismatchError {
                    span_total: todo!(),
                    span1: todo!(),
                    typ1: todo!(),
                    span2: todo!(),
                    typ2: todo!(),
                }));
            }
            typ = declared_type;
        } else {
            typ = AstTyp::Error;
        }

        self.vars.set(lhs, place, typ);
        self.emit(Inst::Alloca(place, convert_ast_typ(typ)));
    }
}

fn convert_ast_typ(t: AstTyp) -> Typ {
    match t {
        AstTyp::I64 => Typ::I64,
        AstTyp::Bool => Typ::Bool,
        AstTyp::Error => panic!("tried to convert ast::Typ::Error -> mir::Typ"),
    }
}
