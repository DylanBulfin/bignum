use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct BigNumError {
    kind: BigNumErrorKind,
    message: String,
}

#[derive(Debug)]
pub enum BigNumErrorKind {
    None,
}

impl Display for BigNumErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "{}",
            match self {
                BigNumErrorKind::None => "None",
            }
        ))
    }
}

impl Display for BigNumError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!(
            "ErrorKind: {}, Message: {}",
            self.kind, self.message
        ))
    }
}

impl From<String> for BigNumError {
    fn from(message: String) -> Self {
        Self {
            kind: BigNumErrorKind::None,
            message,
        }
    }
}

impl From<&str> for BigNumError {
    fn from(message: &str) -> Self {
        Self::from(message.to_string())
    }
}

pub type BigNumResult<T> = Result<T, BigNumError>;
pub type BigNumTestResult = Result<(), BigNumError>;
