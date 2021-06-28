use crate::token::Token;

use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Expr {
    Ident(Token),
    Literal(Literal),
    Prefix(Token, Box<Expr>),
    Infix(Token, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Stmt {
    Let(Expr, Expr),
    Return(Expr),
    Expr(Expr),
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Literal {
    Int(i64),
    Bool(bool),
}

impl fmt::Display for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Let(lhs, rhs) => write!(f, "let {} == {};", lhs, rhs),
            Self::Return(expr) => write!(f, "return {};", expr),
            Self::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Ident(ident) => write!(f, "{}", ident),
            Self::Literal(literal) => write!(f, "{}", literal),
            Self::Prefix(op, expr) => write!(f, "({}{})", op, expr),
            Self::Infix(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
        }
    }
}

impl fmt::Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{}", int),
            Self::Bool(b) => write!(f, "{}", b),
        }
    }
}
