use monkey::lexer::Lexer;

use std::io::{self, Read, Write};
use std::process;

fn main() {
    repl();
}

fn repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut input = String::new();

    loop {
        print!(">> ");
        let _ = stdout.flush();

        stdin.read_line(&mut input).unwrap();

        if input.trim() == "exit()" {
            process::exit(0);
        }

        let mut lexer = Lexer::new(&input);
        for token in lexer {
            println!("{:?}", token);
        }

        input.clear();
    }
}
