use std::io;

use ariadne::{Report, ReportKind};
use internment::Intern;

use crate::errors::{MugErr, Span};

#[derive(Debug)]
pub struct MugIoError {
    name: String,
    err: io::Error,
}

impl MugIoError {
    pub fn report(self) -> Report<'static, Span> {
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

impl PartialEq for MugIoError {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.err.kind() == other.err.kind()
    }
}

pub fn reading_file(filename: &str, err: io::Error) -> Vec<MugErr> {
    vec![MugErr::Io(MugIoError {
        err,
        name: filename.to_string(),
    })]
}
