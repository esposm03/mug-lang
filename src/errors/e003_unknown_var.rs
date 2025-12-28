use ariadne::{Label, Report, ReportKind};

use crate::{
    errors::{MugError, Span},
    parsing::ast::{Ident, Spanned},
};

pub struct UnknownVarError(pub Spanned<Ident>);

impl MugError for UnknownVarError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        Report::build(ReportKind::Error, self.0.span)
            .with_code(3)
            .with_message(format!("Unknown variable `{}`", self.0.t))
            .with_label(Label::new(self.0.span).with_message("Unknown variable"))
            .finish()
    }
}
