use crate::ast::{Expr, Literal, Stmt};
use crate::errors::ParsingError;
use crate::lexer::Lexer;
use crate::token::Token;

pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
    curr: Token,
    next: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let curr = Token::EOF;
        let next = Token::EOF;

        let mut parser = Self { lexer, curr, next };

        parser.advance();
        parser.advance();
        parser
    }

    fn parse(&mut self) -> (Vec<ParsingError>, Vec<Stmt>) {
        let mut program: Vec<Stmt> = Vec::new();
        let mut errors: Vec<ParsingError> = Vec::new();

        while self.curr != Token::EOF {
            match self.parse_stmt() {
                Ok(ast) => program.push(ast),
                Err(error) => errors.push(error),
            }
        }

        (errors, program)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParsingError> {
        match self.curr {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),

            _ => self.parse_expr_stmt(),
        }
    }

    /// Parse a let statement.
    /// LET IDENT = <expr>;
    fn parse_let_stmt(&mut self) -> Result<Stmt, ParsingError> {
        // Advance beyond the LET token if next token is an identifier.
        self.advance_if_next(Token::Ident(String::new()))?;

        // Construct the IDENT node.
        let ident = Expr::Ident(self.curr.clone());

        // Advance beyond the IDENT token if next is an ASSIGN token.
        self.advance_if_next(Token::Assign)?;

        // Advance beyond ASSIGN.
        self.advance();

        // Consume the expression.
        let expr = self.parse_expr()?;

        // Consume a semicolon if it exists, or return an error.
        self.advance_if_curr(Token::SemiColon)?;

        Ok(Stmt::Let(ident, expr))
    }

    /// Parse a return statement.
    /// RETURN <expr>;
    fn parse_return_stmt(&mut self) -> Result<Stmt, ParsingError> {
        // Advance beyond the RETURN token.
        self.advance();

        // Parse the expression.
        let expr = self.parse_expr()?;

        // Advance beyond expression if next is a SEMICOLON token.
        self.advance_if_curr(Token::SemiColon)?;

        Ok(Stmt::Return(expr))
    }

    /// Parse an expression statement.
    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParsingError> {
        let expr = self.parse_expr()?;

        // Optional semi-colon.
        self.advance_if_curr(Token::SemiColon);

        Ok(Stmt::Expr(expr))
    }

    /// Parse an expression.
    fn parse_expr(&mut self) -> Result<Expr, ParsingError> {
        self.parse_subexpr(0)
    }

    /// Helper function used by `parse_expr()`.
    /// Parse an expression until operand with greater precedence than `min_bp` is found.
    fn parse_subexpr(&mut self, min_bp: u8) -> Result<Expr, ParsingError> {
        let tok = self.curr.clone();
        self.advance();

        let mut lhs = match tok {
            // Atoms
            Token::Ident(_) => Expr::Ident(tok),
            Token::Int(i) => Expr::Literal(Literal::Int(i)),

            // Parentheses
            Token::LParen => {
                let expr = self.parse_subexpr(0)?;
                self.advance_if_next(Token::RParen)?;
                expr
            }

            // Prefixes
            Token::Bang | Token::Minus => {
                let prefix = tok;
                let bp = Parser::get_prefix_bp(&prefix)?;

                let expr = self.parse_subexpr(bp)?;

                Expr::Prefix(prefix, Box::new(expr))
            }

            _ => {
                return Err(ParsingError::InvalidOperand(format!(
                    "Invalid operand ('{}') found.",
                    self.curr
                )));
            }
        };

        // Infix
        while !self.curr_is(Token::EOF) & !self.curr_is(Token::SemiColon) {
            let op = self.curr.clone();

            let (l_bp, r_bp) = Parser::get_infix_bp(&op)?;
            if l_bp < min_bp {
                break;
            }

            // Advance beyond the operator
            self.advance();

            let rhs = self.parse_subexpr(r_bp)?;

            lhs = Expr::Infix(op, Box::new(lhs), Box::new(rhs));
        }

        Ok(lhs)
    }

    fn get_prefix_bp(op: &Token) -> Result<u8, ParsingError> {
        let bp = match op {
            Token::Bang | Token::Minus => 8,

            _ => {
                return Err(ParsingError::InvalidOperator(format!(
                    "'{}' is not a valid prefix operator.",
                    op
                )))
            }
        };

        Ok(bp)
    }

    fn get_infix_bp(op: &Token) -> Result<(u8, u8), ParsingError> {
        let bps = match op {
            Token::Eq | Token::NotEq => (0, 1),
            Token::LessThan | Token::GreaterThan => (2, 3),
            Token::Plus | Token::Minus => (4, 5),
            Token::Slash | Token::Star => (6, 7),

            _ => {
                return Err(ParsingError::InvalidOperator(format!(
                    "'{}' is not a valid infix operator.",
                    op
                )))
            }
        };

        Ok(bps)
    }

    /// Check if current token has same variant as `token`.
    fn curr_is(&self, token: Token) -> bool {
        std::mem::discriminant(&token) == std::mem::discriminant(&self.curr)
    }

    /// Check if the next token has same variant as `token`.
    fn next_is(&self, token: Token) -> bool {
        std::mem::discriminant(&token) == std::mem::discriminant(&self.next)
    }

    /// Advance the parser so that it looks at the next token.
    fn advance(&mut self) {
        self.curr = std::mem::replace(&mut self.next, self.lexer.next_token());
    }

    /// Advance the parser if the current token has the same variant as `token`.
    fn advance_if_curr(&mut self, token: Token) -> Result<(), ParsingError> {
        if self.curr_is(token.clone()) {
            self.advance();
            return Ok(());
        }

        Err(ParsingError::UnexpectedToken(format!(
            "Expected '{}' but found '{}'",
            token, self.curr,
        )))
    }

    /// Advance the parser if the next token has the same variant as `token`.
    fn advance_if_next(&mut self, token: Token) -> Result<(), ParsingError> {
        if self.next_is(token.clone()) {
            self.advance();
            return Ok(());
        }

        Err(ParsingError::UnexpectedToken(format!(
            "Expected '{}' but found '{}'",
            token, self.next,
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ast::*;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x = 5;
            let y = 10;
            let z = 15;
        "#;

        let received = {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            parser.parse()
        };

        let expected_errors: Vec<ParsingError> = vec![];
        let expected_stmts = vec![
            Stmt::Let(
                Expr::Ident(Token::Ident(String::from("x"))),
                Expr::Literal(Literal::Int(5)),
            ),
            Stmt::Let(
                Expr::Ident(Token::Ident(String::from("y"))),
                Expr::Literal(Literal::Int(10)),
            ),
            Stmt::Let(
                Expr::Ident(Token::Ident(String::from("z"))),
                Expr::Literal(Literal::Int(15)),
            ),
        ];

        assert_eq!(expected_errors, received.0);
        assert_eq!(expected_stmts, received.1);
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
        "#;

        let received = {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            parser.parse()
        };

        let expected_errors: Vec<ParsingError> = vec![];
        let expected_stmts = vec![
            Stmt::Return(Expr::Literal(Literal::Int(5))),
            Stmt::Return(Expr::Literal(Literal::Int(10))),
        ];

        assert_eq!(expected_errors, received.0);
        assert_eq!(expected_stmts, received.1);
    }

    #[test]
    fn test_prefix_expressions() {
        let input = r#"
            !15;
            -15;
        "#;

        let received = {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            parser.parse()
        };

        let expected_errors: Vec<ParsingError> = vec![];
        let expected_stmts = vec![
            Stmt::Expr(Expr::Prefix(
                Token::Bang,
                Box::new(Expr::Literal(Literal::Int(15))),
            )),
            Stmt::Expr(Expr::Prefix(
                Token::Minus,
                Box::new(Expr::Literal(Literal::Int(15))),
            )),
        ];

        assert_eq!(expected_errors, received.0);
        assert_eq!(expected_stmts, received.1);
    }

    #[test]
    fn test_infix_expressions() {
        let input = r#"
            1 + 2 * 3 * 4 + 5;
        "#;

        let received = {
            let lexer = Lexer::new(input);
            let mut parser = Parser::new(lexer);
            parser.parse()
        };

        let expected_errors: Vec<ParsingError> = vec![];
        let expected_stmts = vec![Stmt::Expr(Expr::Infix(
            Token::Plus,
            Box::new(Expr::Infix(
                Token::Plus,
                Box::new(Expr::Literal(Literal::Int(1))),
                Box::new(Expr::Infix(
                    Token::Star,
                    Box::new(Expr::Infix(
                        Token::Star,
                        Box::new(Expr::Literal(Literal::Int(2))),
                        Box::new(Expr::Literal(Literal::Int(3))),
                    )),
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
            )),
            Box::new(Expr::Literal(Literal::Int(5))),
        ))];

        assert_eq!(expected_errors, received.0);
        assert_eq!(expected_stmts, received.1);
    }

    #[test]
    fn test_expressions() {
        let tests = vec![
            ("!-15", "(!(-15))"),
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
        ];

        for (input, output) in tests {
            let received = {
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);
                parser.parse()
            };

            let expected_errors: Vec<ParsingError> = vec![];

            assert_eq!(received.0, expected_errors);
            assert_eq!(received.1[0].to_string(), output.to_string());
        }
    }
}
