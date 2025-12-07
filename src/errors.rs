use std::fmt::Display;

use ariadne::{Fmt, Label, Report, ReportKind};
use chumsky::error::{Rich, RichReason};

type SpanTy<'a> = (&'a str, std::ops::Range<usize>);

fn str_join_iterator<'a, D: Display, I: ExactSizeIterator<Item = D>>(it: I) -> String {
    use std::fmt::Write;
    let mut res = String::new();

    let len = it.len();
    for (i, exp) in it.enumerate() {
        write!(res, "{exp}").unwrap();

        if i + 2 <= len {
            res.push_str(", ");
        }
        if i + 2 == len {
            res.push_str("or ");
        }
    }

    res
}

pub fn reason_to_msg<T: Display>(s: &RichReason<T>) -> String {
    match s {
        RichReason::ExpectedFound { expected, .. } => {
            if expected.len() == 1 {
                format!("expected {}", expected[0])
            } else {
                format!("expected one of {}", str_join_iterator(expected.iter()))
            }
        }
        RichReason::Custom(s) => s.clone(),
    }
}

pub fn reason_to_label<'a, T: Clone + Display>(
    span: SpanTy<'a>,
    reason: &RichReason<T>,
) -> Label<SpanTy<'a>> {
    let col = ariadne::Color::Red;

    let msg = match reason {
        RichReason::ExpectedFound { found: Some(x), .. } => {
            let tick = "'".fg(col);
            format!("found {tick}{}{tick}", x.to_string().fg(col))
        }
        RichReason::ExpectedFound { found: None, .. } => {
            let eof = "EOF".fg(col);
            format!("found {eof}")
        }
        RichReason::Custom(s) => s.clone(),
    };

    Label::new(span).with_message(msg).with_color(col)
}

#[must_use]
pub fn report_error<'a, T: Clone + Display>(
    filename: &'a str,
    err: Rich<'a, T>,
) -> Report<'a, SpanTy<'a>> {
    let sp = (filename, err.span().start..err.span().end);
    Report::build(ReportKind::Error, sp.clone())
        .with_message(reason_to_msg(err.reason()))
        .with_label(reason_to_label(sp, err.reason()))
        .finish()
}
