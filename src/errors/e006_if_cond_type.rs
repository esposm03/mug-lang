use ariadne::{ColorGenerator, Fmt, Label, Report, ReportKind};

use crate::{errors::Span, parsing::ast::Typ};

#[derive(Debug, PartialEq)]
pub struct ConditionNotBoolError {
    pub is_elif: bool,
    pub found_typ: Typ,
    pub found_span: Span,
}

impl ConditionNotBoolError {
    pub fn report(self) -> Report<'static, Span> {
        let mut colors = ColorGenerator::new();
        let found_typ = self.found_typ.fg(colors.next());
        Report::build(ReportKind::Error, self.found_span)
            .with_code(6)
            .with_message(format!(
                "`{}` conditions must be of type `{}`",
                if self.is_elif { "elif" } else { "if" },
                "bool".fg(colors.next()),
            ))
            .with_label(
                Label::new(self.found_span)
                    .with_message(format!("This if of type `{}`", found_typ,)),
            )
            .finish()
    }
}
