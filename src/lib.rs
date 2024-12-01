use std::{collections::VecDeque, marker::PhantomData};

use lexer::Lexer;

pub mod lexer;

pub struct Parser<'a> {
    _pd: PhantomData<&'a ()>,
}

impl<'a> Parser<'a> {
    pub fn parse(lexer: Lexer<'a>) -> miette::Result<Self> {
        let Lexer { input: _, tokens } = lexer;
        let mut tokens = VecDeque::from(tokens);

        loop {
            let Some(tok) = tokens.pop_front() else {
                break;
            };
        }

        todo!()
    }
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, Parser};

    #[test]
    fn unit_struct() {
        let input = r#"
            struct UnitStruct;
        "#;
        let lex = Lexer::lex(input).unwrap();
        let mut parser = Parser::parse(lex);

    }
}
