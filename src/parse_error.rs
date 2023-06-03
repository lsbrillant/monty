use crate::exceptions::{ExceptionRaise, InternalRunError, RunError};
use std::borrow::Cow;
use std::fmt;

#[derive(Debug, Clone)]
pub enum ParseError<'a> {
    Todo(&'a str),
    Parsing(String),
    Internal(Cow<'a, str>),
    PreEvalExc(ExceptionRaise<'a>),
    PreEvalInternal(InternalRunError),
}

impl<'a> fmt::Display for ParseError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Todo(s) => write!(f, "TODO: {s}"),
            Self::Internal(s) => write!(f, "Internal parsing error: {s}"),
            Self::Parsing(s) => write!(f, "Error parsing AST: {s}"),
            Self::PreEvalExc(s) => write!(f, "Pre eval exception: {s}"),
            Self::PreEvalInternal(s) => write!(f, "Pre eval internal error: {s}"),
        }
    }
}

// TODO change to From
impl<'a> ParseError<'a> {
    pub(crate) fn pre_eval(run_error: RunError<'a>) -> Self {
        match run_error {
            RunError::Exc(e) => Self::PreEvalExc(e),
            RunError::Internal(e) => Self::PreEvalInternal(e),
        }
    }
}

pub type ParseResult<'a, T> = Result<T, ParseError<'a>>;

impl<'a> From<InternalRunError> for ParseError<'a> {
    fn from(internal_run_error: InternalRunError) -> Self {
        Self::PreEvalInternal(internal_run_error)
    }
}
