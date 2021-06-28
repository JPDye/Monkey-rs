use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    Illegal,
    EOF,

    // Identifiers and Literals
    Bool(bool),
    Int(i64),
    Ident(String),

    // Operators
    Assign,
    Plus,
    Minus,
    Bang,
    Star,
    Slash,

    LessThan,
    GreaterThan,

    Eq,
    NotEq,

    // Delimiters
    Comma,
    SemiColon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    // Keywords
    Function,
    Let,
    If,
    Else,
    Return,
}
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{}", b),
            Self::Int(int) => write!(f, "{}", int),
            Self::Ident(ident) => write!(f, "{}", ident),

            Self::Assign => write!(f, "="),
            Self::Plus => write!(f, "+"),
            Self::Minus => write!(f, "-"),
            Self::Bang => write!(f, "!"),
            Self::Star => write!(f, "*"),
            Self::Slash => write!(f, "/"),

            Self::LessThan => write!(f, "<"),
            Self::GreaterThan => write!(f, ">"),

            Self::Eq => write!(f, "=="),
            Self::NotEq => write!(f, "!="),

            Self::Comma => write!(f, ","),
            Self::SemiColon => write!(f, ";"),

            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),

            _ => write!(f, "{:?}", self),
        }
    }
}
