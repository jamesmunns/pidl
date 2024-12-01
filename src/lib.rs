use std::{collections::{HashMap, VecDeque}, hash::Hash, marker::PhantomData};

use lexer::{Lexer, SpanToken, Token};
use postcard_schema::schema::owned::{OwnedDataModelType, OwnedNamedType};

pub mod macros;

pub mod lexer;

pub struct Unresolved<'a>{
    pub waiting_on: Vec<SpanName<'a>>,
    // this is probably wrong
    pub body: Vec<SpanToken<'a>>,
}

pub enum TypeKind<'a> {
    Resolved(OwnedNamedType),
    Unresolved(Unresolved<'a>),
}

#[derive(Clone, Copy)]
pub struct SpanName<'a> {
    span: (usize, usize),
    name: &'a str,
}

impl<'a> PartialEq for SpanName<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(other.name)
    }
}

impl<'a> Eq for SpanName<'a> {

}

impl<'a> Hash for SpanName<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.name.as_bytes());
    }
}

pub struct Parser<'a> {
    input: &'a str,
    types: HashMap<SpanName<'a>, TypeKind<'a>>,
    // todo: Something for partially instantiated types that are
    // still waiting for resolution?
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            types: HashMap::new(),
        }
    }
    pub fn parse(lexer: Lexer<'a>) -> miette::Result<Self> {
        let Lexer { input, tokens } = lexer;
        // remove any comments
        let mut tokens = tokens
            .into_iter()
            .filter(|tok| !matches!(tok.token, Token::Comment(_)))
            .collect::<VecDeque<_>>();

        let mut me = Self::new(lexer.input);

        loop {
            let Some(tok) = tokens.pop_front() else {
                break;
            };
            match tok.token {
                Token::Word("struct") => {
                    me.take_struct(input, &tok, &mut tokens)?;
                }
                _ => todo!(),
            }
        }

        // TODO: make sure all names are resolved

        Ok(me)
    }

    fn take_struct(
        &mut self,
        input: &str,
        struct_tok: &SpanToken,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<()> {
        // We always start with a name
        let Some(nametok) = toks.pop_front() else {
            return Err(MissingStructName::from_input_spantoken(input, struct_tok).into());
        };
        let Token::Word(name) = nametok.token else {
            return Err(BadStructName::from_input_spantoken(input, &nametok).into());
        };
        // todo, validate name?
        let Some(nxttok) = toks.pop_front() else {
            return Err(UnexpectedEnd::from_input_spantoken(input, &nametok).into());
        };
        match nxttok.token {
            Token::Semicolon => {
                let sname = SpanName { span: nametok.span, name };
                self.unit_struct(&nametok, sname)?;
            },
            Token::LeftCurly => todo!("Normal Struct"),
            Token::LeftParen => todo!("Tuple or Newtype Struct"),
            _ => return Err(UnexpectedStructPart::from_input_spantoken(input, &nxttok).into()),
        }

        Ok(())
    }

    fn unit_struct(&mut self, nametok: &SpanToken<'a>, sname: SpanName<'a>) -> miette::Result<()> {
        // Is it a dupe?
        if self.types.contains_key(&sname) {
            return Err(DuplicateName::from_input_spantoken(self.input, nametok).into());
        }
        // We can immediately resolve unit structs
        self.types.insert(sname, TypeKind::Resolved(OwnedNamedType {
            name: sname.name.to_string(),
            ty: OwnedDataModelType::UnitStruct,
        }));

        Ok(())
    }
}

merrors! {
    | Name                  | Error             | Label                     | Help                                      |
    | ----                  | -----             | -----                     | ----                                      |
    | MissingStructName     | "Missing Name!"   | "here"                    | "Expected the name of the struct"         |
    | BadStructName         | "Not a Name!"     | "here"                    | "Expected the name of the struct"         |
    | UnexpectedEnd         | "Unexpected End"  | "Somewhere after here"    | "Unexpected end of file!"                 |
    | UnexpectedStructPart  | "Unexpected text" | "This"                    | "This token was unexpected"               |
    | DuplicateName         | "Duplicate Name"  | "Here"                    | "This was already defined!"               |
}

#[cfg(test)]
mod test {
    use crate::{lexer::Lexer, Parser};

    #[test]
    fn unit_struct() {
        let input = r#"
            struct Bob;
        "#;
        let lex = Lexer::lex(input).unwrap();
        let _parser = Parser::parse(lex).unwrap();
    }

    #[test]
    fn newtype_struct() {
        let input = r#"
            struct NewType(u32);
        "#;
        let lex = Lexer::lex(input).unwrap();
        let _parser = Parser::parse(lex).unwrap();
    }
}
