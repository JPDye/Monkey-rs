#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ParsingError {
    UnexpectedToken(String),
    InvalidOperand(String),
    InvalidOperator(String),
}
