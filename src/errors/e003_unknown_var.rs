use ariadne::{Label, Report, ReportKind};

use crate::{
    errors::Span,
    parsing::ast::{Ident, Spanned},
};

#[derive(Debug, PartialEq)]
pub struct UnknownVarError(pub Spanned<Ident>);

impl UnknownVarError {
    pub fn report(self) -> Report<'static, Span> {
        Report::build(ReportKind::Error, self.0.span)
            .with_code(3)
            .with_message(format!("Unknown variable `{}`", self.0.t))
            .with_label(Label::new(self.0.span).with_message("Unknown variable"))
            .finish()
    }
}
