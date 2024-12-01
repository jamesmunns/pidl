use std::{
    collections::{HashMap, VecDeque},
    hash::Hash,
};

use lexer::{Lexer, SpanToken, Token};
use postcard_schema::schema::owned::{OwnedDataModelType, OwnedNamedType};
use postcard_schema::Schema;

pub mod lexer;
pub mod macros;

pub enum TypeEntry<'a> {
    U32,
    UnitStruct {
        name: SpanName<'a>,
    },
    NewTypeStruct {
        name: SpanName<'a>,
        ty: TextType<'a>,
    },
}

impl<'a> TypeEntry<'a> {
    pub fn name(&self) -> &SpanName<'a> {
        match self {
            TypeEntry::U32 => &SpanName {
                span: None,
                name: "u32",
            },
            TypeEntry::UnitStruct { name } => name,
            TypeEntry::NewTypeStruct { name, ty: _ } => name,
        }
    }

    pub fn span(&self) -> (usize, usize) {
        todo!()
    }
}

#[derive(Debug)]
pub enum TextType<'a> {
    Name(SpanName<'a>),
    // templates and stuff?
}

#[derive(Debug)]
pub struct Parens<'a> {
    pub start: SpanToken<'a>,
    pub end: SpanToken<'a>,
    pub contained: Vec<GroupedToks<'a>>,
}

#[derive(Debug)]
pub struct Square<'a> {
    pub start: SpanToken<'a>,
    pub end: SpanToken<'a>,
    pub contained: Vec<GroupedToks<'a>>,
}

#[derive(Debug)]
pub struct Angle<'a> {
    pub start: SpanToken<'a>,
    pub end: SpanToken<'a>,
    pub contained: Vec<GroupedToks<'a>>,
}

#[derive(Debug)]
pub struct Curly<'a> {
    pub start: SpanToken<'a>,
    pub end: SpanToken<'a>,
    pub contained: Vec<GroupedToks<'a>>,
}

#[derive(Debug)]
pub enum GroupedToks<'a> {
    Parens(Parens<'a>),
    Square(Square<'a>),
    Angle(Angle<'a>),
    Curly(Curly<'a>),
    Token(SpanToken<'a>),
}

// pub struct Unresolved<'a> {
//     pub waiting_on: Vec<SpanName<'a>>,
//     // this is probably wrong
//     pub body: Vec<SpanToken<'a>>,
// }

// pub enum TypeKind<'a> {
//     Resolved(OwnedNamedType),
//     Unresolved(Unresolved<'a>),
// }

fn prim_tys() -> HashMap<SpanName<'static>, TypeEntry<'static>> {
    let mut map = HashMap::new();
    map.insert(
        SpanName {
            span: None,
            name: "u32",
        },
        TypeEntry::U32,
    );
    map
}

#[derive(Debug, Clone, Copy)]
pub struct SpanName<'a> {
    span: Option<(usize, usize)>,
    name: &'a str,
}

impl PartialEq for SpanName<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(other.name)
    }
}

impl Eq for SpanName<'_> {}

impl Hash for SpanName<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(self.name.as_bytes());
    }
}

pub struct Parser<'a> {
    input: &'a str,
    types: HashMap<SpanName<'a>, TypeEntry<'a>>,
    // todo: Something for partially instantiated types that are
    // still waiting for resolution?
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input,
            types: prim_tys(),
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
    ) -> miette::Result<TypeEntry<'a>> {
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
                let sname = SpanName {
                    span: Some(nametok.span),
                    name,
                };
                self.unit_struct(&nametok, sname)
            }
            Token::LeftCurly => todo!("Normal Struct"),
            Token::LeftParen => {
                let sname = SpanName {
                    span: Some(nametok.span),
                    name,
                };
                self.paren_struct(&nametok, sname, nxttok, toks)
            }
            _ => Err(UnexpectedStructPart::from_input_spantoken(input, &nxttok).into()),
        }
    }

    fn paren_struct(
        &mut self,
        nametok: &SpanToken<'a>,
        sname: SpanName<'a>,
        sparen: SpanToken<'a>,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<TypeEntry<'a>> {
        let ptoks = self.take_parens(sparen, toks)?;
        let has_comma = ptoks.contained.iter().any(|tok| {
            matches!(
                tok,
                GroupedToks::Token(SpanToken {
                    span: _,
                    token: Token::Comma
                })
            )
        });
        let res = if has_comma {
            self.tuple_struct(nametok, sname, &ptoks.contained)?
        } else {
            self.newtype_struct(nametok, sname, &ptoks.contained)?
        };
        let semi = toks.pop_front();
        match semi {
            Some(SpanToken { span: _, token: Token::Semicolon }) => Ok(res),
            Some(other) => Err(ExpectedSemiColon::from_input_spantoken(self.input, &other).into()),
            None => Err(ExpectedSemiColon::from_type_entry(self.input, &res).into())
        }
    }

    fn tuple_struct(
        &mut self,
        nametok: &SpanToken<'a>,
        sname: SpanName<'a>,
        toks: &[GroupedToks<'a>],
    ) -> miette::Result<TypeEntry<'a>> {
        todo!()
    }

    fn newtype_struct(
        &mut self,
        nametok: &SpanToken<'a>,
        sname: SpanName<'a>,
        toks: &[GroupedToks<'a>],
    ) -> miette::Result<TypeEntry<'a>> {
        let (ty, remain) = self.take_one_type(toks)?;
        if !remain.is_empty() {
            panic!("extra tokens? {remain:?}");
        }
        Ok(TypeEntry::NewTypeStruct { name: sname, ty })
    }

    fn unit_struct(
        &mut self,
        nametok: &SpanToken<'a>,
        sname: SpanName<'a>,
    ) -> miette::Result<TypeEntry<'a>> {
        // Is it a dupe?
        if self.types.contains_key(&sname) {
            return Err(DuplicateName::from_input_spantoken(self.input, nametok).into());
        }
        // We COULD immediately resolve unit structs, but we wont I guess?
        Ok(TypeEntry::UnitStruct { name: sname })
    }

    fn take_one_type<'b>(
        &mut self,
        toks: &'b [GroupedToks<'a>],
    ) -> miette::Result<(TextType<'a>, &'b [GroupedToks<'a>])> {
        match toks {
            [GroupedToks::Parens(p), remain @ ..] => {
                // Tuple
                panic!("Tuple")
            }
            [GroupedToks::Square(s), remain @ ..] => {
                // array
                panic!("array")
            }
            [GroupedToks::Token(SpanToken {
                span: _,
                token: Token::Word(w),
            }), GroupedToks::Angle(ang), remain @ ..] => {
                // template
                panic!("template")
            }
            [GroupedToks::Token(SpanToken {
                span,
                token: Token::Word(w),
            }), remain @ ..] => {
                let name = SpanName {
                    span: Some(*span),
                    name: w,
                };
                Ok((TextType::Name(name), remain))
            }
            _ => panic!(),
        }
    }

    fn take_angles(
        &mut self,
        start: SpanToken<'a>,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<Angle<'a>> {
        let mut ptoks = vec![];
        let end;
        loop {
            let Some(tok) = toks.pop_front() else {
                // EOF before we match parens
                return Err(UnmatchedAngle::from_input_spantoken(self.input, &start).into());
            };
            match tok.token {
                // A start of a pair means we need to recurse
                Token::LeftAngle => {
                    let angles = self.take_angles(tok, toks)?;
                    ptoks.push(GroupedToks::Angle(angles));
                }
                Token::LeftCurly => {
                    let curlies = self.take_curlies(tok, toks)?;
                    ptoks.push(GroupedToks::Curly(curlies));
                }
                Token::LeftSquare => {
                    let squares = self.take_squares(tok, toks)?;
                    ptoks.push(GroupedToks::Square(squares));
                }
                Token::LeftParen => {
                    let parens = self.take_parens(tok, toks)?;
                    ptoks.push(GroupedToks::Parens(parens));
                }

                // I don't think we should ever see right pairs, they should be handled
                Token::RightSquare => unreachable!(),
                Token::RightCurly => unreachable!(),
                Token::RightParen => unreachable!(),

                // Comments should already be stripped
                Token::Comment(_) => unreachable!(),

                // This is the end for us
                Token::RightAngle => {
                    end = tok;
                    break;
                }

                _other => {
                    ptoks.push(GroupedToks::Token(tok));
                }
            }
        }
        Ok(Angle {
            start,
            end,
            contained: ptoks,
        })
    }

    fn take_curlies(
        &mut self,
        start: SpanToken<'a>,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<Curly<'a>> {
        let mut ptoks = vec![];
        let end;
        loop {
            let Some(tok) = toks.pop_front() else {
                // EOF before we match parens
                return Err(UnmatchedCurly::from_input_spantoken(self.input, &start).into());
            };
            match tok.token {
                // A start of a pair means we need to recurse
                Token::LeftAngle => {
                    let angles = self.take_angles(tok, toks)?;
                    ptoks.push(GroupedToks::Angle(angles));
                }
                Token::LeftCurly => {
                    let curlies = self.take_curlies(tok, toks)?;
                    ptoks.push(GroupedToks::Curly(curlies));
                }
                Token::LeftSquare => {
                    let squares = self.take_squares(tok, toks)?;
                    ptoks.push(GroupedToks::Square(squares));
                }
                Token::LeftParen => {
                    let parens = self.take_parens(tok, toks)?;
                    ptoks.push(GroupedToks::Parens(parens));
                }

                // I don't think we should ever see right pairs, they should be handled
                Token::RightSquare => unreachable!(),
                Token::RightAngle => unreachable!(),
                Token::RightParen => unreachable!(),

                // Comments should already be stripped
                Token::Comment(_) => unreachable!(),

                // This is the end for us
                Token::RightCurly => {
                    end = tok;
                    break;
                }

                _other => {
                    ptoks.push(GroupedToks::Token(tok));
                }
            }
        }
        Ok(Curly {
            start,
            end,
            contained: ptoks,
        })
    }

    fn take_squares(
        &mut self,
        start: SpanToken<'a>,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<Square<'a>> {
        let mut ptoks = vec![];
        let end;
        loop {
            let Some(tok) = toks.pop_front() else {
                // EOF before we match parens
                return Err(UnmatchedCurly::from_input_spantoken(self.input, &start).into());
            };
            match tok.token {
                // A start of a pair means we need to recurse
                Token::LeftAngle => {
                    let angles = self.take_angles(tok, toks)?;
                    ptoks.push(GroupedToks::Angle(angles));
                }
                Token::LeftCurly => {
                    let curlies = self.take_curlies(tok, toks)?;
                    ptoks.push(GroupedToks::Curly(curlies));
                }
                Token::LeftSquare => {
                    let squares = self.take_squares(tok, toks)?;
                    ptoks.push(GroupedToks::Square(squares));
                }
                Token::LeftParen => {
                    let parens = self.take_parens(tok, toks)?;
                    ptoks.push(GroupedToks::Parens(parens));
                }

                // I don't think we should ever see right pairs, they should be handled
                Token::RightAngle => unreachable!(),
                Token::RightCurly => unreachable!(),
                Token::RightParen => unreachable!(),

                // Comments should already be stripped
                Token::Comment(_) => unreachable!(),

                // This is the end for us
                Token::RightSquare => {
                    end = tok;
                    break;
                }

                _other => {
                    ptoks.push(GroupedToks::Token(tok));
                }
            }
        }
        Ok(Square {
            start,
            end,
            contained: ptoks,
        })
    }

    fn take_parens(
        &mut self,
        start: SpanToken<'a>,
        toks: &mut VecDeque<SpanToken<'a>>,
    ) -> miette::Result<Parens<'a>> {
        let mut ptoks = vec![];
        let end;
        loop {
            let Some(tok) = toks.pop_front() else {
                // EOF before we match parens
                return Err(UnmatchedCurly::from_input_spantoken(self.input, &start).into());
            };
            match tok.token {
                // A start of a pair means we need to recurse
                Token::LeftAngle => {
                    let angles = self.take_angles(tok, toks)?;
                    ptoks.push(GroupedToks::Angle(angles));
                }
                Token::LeftCurly => {
                    let curlies = self.take_curlies(tok, toks)?;
                    ptoks.push(GroupedToks::Curly(curlies));
                }
                Token::LeftSquare => {
                    let squares = self.take_squares(tok, toks)?;
                    ptoks.push(GroupedToks::Square(squares));
                }
                Token::LeftParen => {
                    let parens = self.take_parens(tok, toks)?;
                    ptoks.push(GroupedToks::Parens(parens));
                }

                // I don't think we should ever see right pairs, they should be handled
                Token::RightAngle => unreachable!(),
                Token::RightCurly => unreachable!(),
                Token::RightSquare => unreachable!(),

                // Comments should already be stripped
                Token::Comment(_) => unreachable!(),

                // This is the end for us
                Token::RightParen => {
                    end = tok;
                    break;
                }

                _other => {
                    ptoks.push(GroupedToks::Token(tok));
                }
            }
        }
        Ok(Parens {
            start,
            end,
            contained: ptoks,
        })
    }
}

merrors! {
    | Name                  | Error                     | Label                     | Help                                      |
    | ----                  | -----                     | -----                     | ----                                      |
    | MissingStructName     | "Missing Name!"           | "here"                    | "Expected the name of the struct"         |
    | BadStructName         | "Not a Name!"             | "here"                    | "Expected the name of the struct"         |
    | UnexpectedEnd         | "Unexpected End"          | "Somewhere after here"    | "Unexpected end of file!"                 |
    | UnexpectedStructPart  | "Unexpected text"         | "This"                    | "This token was unexpected"               |
    | DuplicateName         | "Duplicate Name"          | "Here"                    | "This was already defined!"               |
    | UnmatchedParen        | "Unmatched '('"           | "Starting Here"           | "There seems to be a missing ')'"         |
    | UnmatchedSquare       | "Unmatched '['"           | "Starting Here"           | "There seems to be a missing ']'"         |
    | UnmatchedAngle        | "Unmatched '<'"           | "Starting Here"           | "There seems to be a missing '>'"         |
    | UnmatchedCurly        | "Unmatched '{{'"          | "Starting Here"           | "There seems to be a missing '}}'"        |
    | ExpectedSemiColon     | "Expected ';'"            | "After Here"              | "Expected a semicolon after this item"    |
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
