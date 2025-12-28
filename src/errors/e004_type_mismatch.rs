use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};

use crate::{
    errors::{MugError, Span},
    parsing::ast::Typ,
};

pub struct TypeMismatchError {
    pub span_total: Span,
    pub span1: Span,
    pub typ1: Typ,
    pub span2: Span,
    pub typ2: Typ,
}

impl MugError for TypeMismatchError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        let mut colors = ColorGenerator::new();
        Report::build(ReportKind::Error, self.span_total)
            .with_code(4)
            .with_message("Incompatible types")
            .with_label(
                Label::new(self.span1)
                    .with_message(format!("This is of type {}", self.typ1.fg(colors.next()))),
            )
            .with_label(
                Label::new(self.span2)
                    .with_message(format!("This is of type {}", self.typ2.fg(colors.next()))),
            )
            .finish()
    }
}
