use crate::token::Token;

pub struct Lexer<'a> {
    input: &'a str,
    curr_pos: usize,
    read_pos: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            curr_pos: 0,
            read_pos: 0,
            ch: 0,
        };

        lexer.read_byte();
        lexer
    }

    pub fn next_token(&mut self) -> Token {
        self.consume_whitespace();

        let token = match self.ch as char {
            '!' => self.match_next('=', Token::NotEq).unwrap_or(Token::Bang),
            '=' => self.match_next('=', Token::Eq).unwrap_or(Token::Assign),

            '<' => Token::LessThan,
            '>' => Token::GreaterThan,

            '+' => Token::Plus,
            '*' => Token::Star,
            '-' => Token::Minus,
            '/' => Token::Slash,

            ';' => Token::SemiColon,
            ',' => Token::Comma,

            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,

            'a'..='z' | 'A'..='Z' | '_' => self.consume_identifier(),

            '0'..='9' => self.consume_number(),

            '\x00' => Token::EOF,
            _ => Token::Illegal,
        };

        self.read_byte();
        token
    }

    // Read as many valid identifier characters (letters and underscores) into a token.
    fn consume_identifier(&mut self) -> Token {
        let start_pos = self.curr_pos;

        while let 'a'..='z' | 'A'..='Z' | '_' = self.peek_byte() as char {
            self.read_byte();
        }

        match &self.input[start_pos..self.read_pos] {
            "if" => Token::If,
            "else" => Token::Else,

            "true" => Token::Bool(true),
            "false" => Token::Bool(false),

            "fn" => Token::Function,
            "let" => Token::Let,

            "return" => Token::Return,
            identifier => Token::Ident(String::from(identifier)),
        }
    }

    // Read as many digits as possible into a token.
    fn consume_number(&mut self) -> Token {
        let start_pos = self.curr_pos;

        while self.peek_byte().is_ascii_digit() {
            self.read_byte();
        }

        let number = &self.input[start_pos..self.read_pos];
        Token::Int(number.parse().unwrap())
    }

    // Read and discard as much whitespace as possible.
    fn consume_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_byte();
        }
    }

    // Match next byte against given character. If match, consume and return Some(t).
    fn match_next(&mut self, c: char, t: Token) -> Option<Token> {
        if self.peek_byte() as char == c {
            self.read_byte();
            Some(t)
        } else {
            None
        }
    }

    // Read the next byte in the input string. If at end of file, set byte to 0.
    fn read_byte(&mut self) {
        self.ch = *self.input.as_bytes().get(self.read_pos).unwrap_or(&0);
        self.curr_pos = self.read_pos;
        self.read_pos += 1;
    }

    // Peek at the next byte in the input string. If at the end of file, set byte to 0.
    fn peek_byte(&mut self) -> u8 {
        *self.input.as_bytes().get(self.read_pos).unwrap_or(&0)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.next_token() {
            Token::EOF => None,
            token => Some(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::Token;

    #[test]
    #[rustfmt::skip]
    fn test_next_token() {
        let input = r#"
            let five = 5 ;
            let ten = 10;

            let add = fn(x, y) {
                x + y;
            };

            let result = add(five, ten);

            !-/*5;
            5 < 10 > 5;
            
            if (5 < 10) {
                return true;
            } else {
                return false;
            }
            
            10 == 10;
            10 != 9;
        "#;

        let received: Vec<Token> = Lexer::new(input).collect();
        let expected = vec![
            // let five = 5;
            Token::Let,
            Token::Ident(String::from("five")),
            Token::Assign,
            Token::Int(5),
            Token::SemiColon,
            
            // let ten = 10;
            Token::Let,
            Token::Ident(String::from("ten")),
            Token::Assign,
            Token::Int(10),
            Token::SemiColon,
            
            // let add = fn(x, y) {
            Token::Let,
            Token::Ident(String::from("add")),
            Token::Assign,
            Token::Function,
            Token::LParen,
            Token::Ident(String::from("x")),
            Token::Comma,
            Token::Ident(String::from("y")),
            Token::RParen,
            Token::LBrace,
            
            // x + y;
            Token::Ident(String::from("x")),
            Token::Plus,
            Token::Ident(String::from("y")),
            Token::SemiColon,
            
            // };
            Token::RBrace,
            Token::SemiColon,
            
            // let result = add(five, ten);
            Token::Let,
            Token::Ident(String::from("result")),
            Token::Assign,
            Token::Ident(String::from("add")),
            Token::LParen,
            Token::Ident(String::from("five")),
            Token::Comma,
            Token::Ident(String::from("ten")),
            Token::RParen,
            Token::SemiColon,

            // !-/*5;
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Star,
            Token::Int(5),
            Token::SemiColon,

            // 5 < 10 > 5;
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::GreaterThan,
            Token::Int(5),
            Token::SemiColon,
            
            // if (5 < 10) {
            Token::If,
            Token::LParen,
            Token::Int(5),
            Token::LessThan,
            Token::Int(10),
            Token::RParen,
            Token::LBrace,
            
            // return true;
            Token::Return,
            Token::Bool(true),
            Token::SemiColon,
            
            // } else {
            Token::RBrace,
            Token::Else,
            Token::LBrace,
            
            // return false,
            Token::Return,
            Token::Bool(false),
            Token::SemiColon,
            
            // }
            Token::RBrace,
            
            // 10 == 10;
            Token::Int(10),
            Token::Eq,
            Token::Int(10),
            Token::SemiColon,
            
            // 10 != 9;
            Token::Int(10),
            Token::NotEq,
            Token::Int(9),
            Token::SemiColon,

        ];

        assert_eq!(expected, received);
    }
}
