use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};

use crate::{
    errors::{MugError, Span},
    parsing::ast::{BinOp, Typ},
};

pub struct BinopTypeMismatchError {
    pub span_total: Span,
    pub op: BinOp,
    pub arg_types: Typ,
    pub expected: Typ,
}

impl MugError for BinopTypeMismatchError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        let mut colors = ColorGenerator::new();
        let expected = self.expected.fg(colors.next());
        let found = self.arg_types.fg(colors.next());
        Report::build(ReportKind::Error, self.span_total)
            .with_code(5)
            .with_message(format!(
                "Can't use the `{}` operator on arguments of type `{}`",
                self.op, found,
            ))
            .with_label(Label::new(self.span_total).with_message(format!(
                "No implementation for `{} {} {}`",
                found, self.op, found,
            )))
            .with_note(format!("Expected arguments to have type {}", expected))
            .finish()
    }
}
