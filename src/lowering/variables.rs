use crate::{
    errors::{Span, TypeMismatchError},
    lowering::{Lower, convert_ast_typ},
    mir::{Inst, Reg},
    parsing::ast::{Expr, Ident, Spanned, Typ, TypCompatible},
};

impl Lower {
    pub fn lower_var_decl(
        &mut self,
        lhs: Spanned<Ident>,
        ty: Option<Spanned<Typ>>,
        rhs: &Expr,
    ) -> (Typ, Span) {
        let rhs_expr = self.mir.temp();
        let (mut typ, rhs_span) = self.lower_expr(rhs, rhs_expr);
        let place = self.mir.place(&lhs.t.0);

        if let Some(Spanned { t, span }) = ty {
            if !typ.compatible(t) {
                self.err(TypeMismatchError {
                    span_total: lhs.span,
                    span1: span,
                    typ1: t,
                    span2: rhs_span,
                    typ2: typ,
                });
            }
            typ = t;
        }

        self.vars.set(lhs, place, typ);
        if typ.is_valid() {
            self.emit(Inst::Alloca(place, convert_ast_typ(typ)));
            self.emit(Inst::Store(place, convert_ast_typ(typ), rhs_expr));
        }

        (Typ::Unit, lhs.span)
    }

    pub fn lower_val_read(&mut self, dest: Reg, ident: Spanned<Ident>) -> (Typ, Span) {
        let v = self.vars.get(&ident).unwrap();
        let res = (v.typ, ident.span);
        if v.typ.is_valid() {
            self.emit(Inst::Load(dest, convert_ast_typ(v.typ), v.place));
        }
        res
    }

    pub fn lower_val_write(
        &mut self,
        lhs: &Spanned<Ident>,
        rhs: &Box<Expr>,
        loc: &Span,
    ) -> (Typ, Span) {
        let tmp = self.mir.temp();
        let (rhs_typ, rhs_span) = self.lower_expr(rhs, tmp);

        if let Some(v) = self.get_var(lhs) {
            if !v.typ.compatible(rhs_typ) {
                self.err(TypeMismatchError {
                    span_total: *loc,
                    span1: lhs.span,
                    typ1: v.typ,
                    span2: rhs_span,
                    typ2: rhs_typ,
                });
            }

            self.emit(Inst::Store(v.place, convert_ast_typ(rhs_typ), tmp));
        }

        (Typ::Unit, *loc)
    }
}

#[cfg(test)]
mod tests {
    use internment::Intern;

    use super::*;

    struct SpanGen(usize);
    impl SpanGen {
        pub fn new() -> Self {
            Self(0)
        }

        pub fn span(&mut self) -> Span {
            self.0 += 1;
            Span {
                source: Intern::from_ref("test.mug"),
                start: self.0,
                end: self.0,
            }
        }

        pub fn ident(&mut self, txt: &str) -> Spanned<Ident> {
            Spanned {
                t: Ident(Intern::from_ref(txt)),
                span: self.span(),
            }
        }

        pub fn typ(&mut self, t: Typ) -> Spanned<Typ> {
            Spanned {
                span: self.span(),
                t,
            }
        }

        pub fn lit_false(&mut self) -> Expr {
            Expr::Bool(false, self.span())
        }

        pub fn lit_int(&mut self, i: i64) -> (Expr, Span) {
            let span = self.span();
            (Expr::Int(i, span), span)
        }
    }

    #[test]
    fn declare_one_inferred() {
        let mut state = Lower::new();
        let mut astgen = SpanGen::new();
        let ident = astgen.ident("hello");

        state.lower_var_decl(ident, None, &astgen.lit_false());
        let var = state.get_var(&ident).unwrap();
        assert_eq!(var.typ, Typ::Bool);
        assert_eq!(var.declared_at, ident.span);
        assert_eq!(state.errors.len(), 0);
    }

    #[test]
    fn declare_one_typed() {
        let mut state = Lower::new();
        let mut astgen = SpanGen::new();
        let ident = astgen.ident("hello");

        state.lower_var_decl(ident, Some(astgen.typ(Typ::Bool)), &astgen.lit_false());
        let var = state.get_var(&ident).unwrap();
        assert_eq!(var.typ, Typ::Bool);
        assert_eq!(var.declared_at, ident.span);
        assert_eq!(state.errors.len(), 0);
    }

    #[test]
    fn declare_one_mistyped() {
        let mut state = Lower::new();
        let mut astgen = SpanGen::new();
        let ident = astgen.ident("hello");
        let typ = astgen.typ(Typ::Bool);
        let rhs = astgen.lit_int(0);

        state.lower_var_decl(ident, Some(typ), &rhs.0);
        let var = state.get_var(&ident).unwrap();
        assert_eq!(var.typ, Typ::Bool);
        assert_eq!(var.declared_at, ident.span);
        assert_eq!(state.errors.len(), 1);

        assert_eq!(
            state.errors.pop().unwrap(),
            From::from(TypeMismatchError {
                span_total: ident.span,
                span1: typ.span,
                typ1: Typ::Bool,
                span2: rhs.1,
                typ2: Typ::I64,
            }),
        );
    }

    #[test]
    fn declare_shadowing() {
        let mut state = Lower::new();
        let mut astgen = SpanGen::new();

        let ident1 = astgen.ident("hello");
        state.lower_var_decl(ident1, None, &astgen.lit_false());
        let var1 = state.get_var(&ident1).unwrap();
        assert_eq!(var1.typ, Typ::Bool);
        assert_eq!(var1.declared_at, ident1.span);
        assert_eq!(state.errors.len(), 0);

        let ident2 = astgen.ident("hello");
        state.lower_var_decl(ident2, None, &astgen.lit_false());
        let var2 = state.get_var(&ident2).unwrap();
        assert_eq!(var2.typ, Typ::Bool);
        assert_eq!(var2.declared_at, ident2.span);
        assert_ne!(var1.place, var2.place);
        assert_eq!(state.errors.len(), 0);
    }
}
