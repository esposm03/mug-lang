use std::io;

use ariadne::{Report, ReportKind};
use internment::Intern;

use crate::errors::{MugErr, MugError, Span};

pub struct MugIoError {
    name: String,
    err: io::Error,
}

impl MugError for MugIoError {
    fn report(self: Box<Self>) -> Report<'static, Span> {
        ariadne::Report::build(
            ReportKind::Error,
            Span {
                source: Intern::from_ref(&self.name),
                start: 0,
                end: 0,
            },
        )
        .with_code(1)
        .with_message(format!("Failed to read file `{}`: {}", self.name, self.err))
        .finish()
    }
}

pub fn reading_file(filename: &str, err: io::Error) -> Vec<MugErr> {
    vec![Box::new(MugIoError {
        err,
        name: filename.to_string(),
    })]
}
