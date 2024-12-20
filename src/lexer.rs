use crate::merrors;

merrors! {
    | Name                  | Error             | Label                 | Help                                      |
    | ----                  | -----             | -----                 | ----                                      |
    | BadDigit              | "Bad Digit!"      | "Unexpected input"    | "Expected Rust-like number"               |
    | UnknownTopLevelToken  | "Unknown Token!"  | "Unexpected input"    | "Expected 'alias', 'struct', or 'enum'"   |
}

#[derive(Debug, Clone, Copy)]
pub struct SpanToken<'a> {
    // offset, len
    pub span: (usize, usize),
    pub token: Token<'a>,
}

#[derive(Debug, Clone, Copy)]
pub enum Token<'a> {
    Word(&'a str),
    Comment(&'a str),
    // todo: do we ever need non-usize numbers?
    Number(usize),
    LeftAngle,
    LeftCurly,
    LeftSquare,
    LeftParen,
    RightAngle,
    RightCurly,
    RightSquare,
    RightParen,
    Semicolon,
    Comma,
    Equals,
    Colon,
}

pub struct Lexer<'a> {
    pub input: &'a str,
    pub tokens: Vec<SpanToken<'a>>,
}

pub fn get_offset(src: &str, sub: &str) -> Option<(usize, usize)> {
    let src_addr = src.as_ptr() as usize;
    let sub_addr = sub.as_ptr() as usize;
    if !(src_addr..(src_addr + src.len())).contains(&sub_addr) {
        return None;
    }
    Some((sub_addr - src_addr, sub.len()))
}

impl<'a> Lexer<'a> {
    pub fn lex(input: &'a str) -> miette::Result<Self> {
        let mut tokens = vec![];
        let mut window = input;
        loop {
            window = window.trim_start();
            let Some(fch) = window.chars().next() else {
                break;
            };
            // is this an interesting single char?
            let taken = match fch {
                '<' => Some(Token::LeftAngle),
                '>' => Some(Token::RightAngle),
                '[' => Some(Token::LeftSquare),
                ']' => Some(Token::RightSquare),
                '{' => Some(Token::LeftCurly),
                '}' => Some(Token::RightCurly),
                '(' => Some(Token::LeftParen),
                ')' => Some(Token::RightParen),
                ',' => Some(Token::Comma),
                ':' => Some(Token::Colon),
                ';' => Some(Token::Semicolon),
                '=' => Some(Token::Equals),
                _ => None,
            };
            if let Some(t) = taken {
                let (now, later) = window.split_at(1);
                tokens.push(SpanToken {
                    span: get_offset(input, now).unwrap(),
                    token: t,
                });
                window = later;
                continue;
            }
            // Okay, no interesting punctuation, let's check the slightly
            // more complicated set of things:
            if fch.is_alphabetic() || fch == '_' {
                // Starting with a letter or underscore smells like a word
                let (name, later) = take_name(input, window);
                tokens.push(name);
                window = later;
            } else if fch.is_numeric() {
                // gather up the numbers
                let (num, later) = take_num(input, window)?;
                tokens.push(num);
                window = later;
            } else if window.starts_with("//") {
                // gather up comments
                let (comment, later) = take_comment_line(input, window);
                tokens.push(comment);
                window = later;
            } else {
                // idk
                let line = window.lines().next().unwrap();
                return Err(UnknownTopLevelToken::new(input, line).into());
            }
        }
        Ok(Self { input, tokens })
    }
}

fn take_name<'a>(input: &'a str, window: &'a str) -> (SpanToken<'a>, &'a str) {
    let end = window
        .char_indices()
        .find(|(_idx, ch)| !(ch.is_alphanumeric() || *ch == '_'));
    if let Some((pos, _ch)) = end {
        let (now, later) = window.split_at(pos);
        let span = get_offset(input, now).unwrap();
        (
            SpanToken {
                span,
                token: Token::Word(now),
            },
            later,
        )
    } else {
        let span = get_offset(input, window).unwrap();
        (
            SpanToken {
                span,
                token: Token::Word(window),
            },
            "",
        )
    }
}

fn take_comment_line<'a>(input: &'a str, window: &'a str) -> (SpanToken<'a>, &'a str) {
    let window2 = window.trim_start_matches('/');
    let window2 = window2.trim_start_matches(' ');
    let window2 = window2.trim_start_matches('\t');
    let (offset, _) = get_offset(window, window2).unwrap();

    let line = window2.lines().next();
    if let Some(line) = line {
        let used = offset + line.len();
        let line = line.trim();
        let span = get_offset(input, line).unwrap();
        let token = Token::Comment(line);
        // the actual newline will get swallowed by the next trim whitespace
        (SpanToken { span, token }, &window[used..])
    } else {
        unreachable!("dont give me empty lines");
    }
}

fn take_num<'a>(input: &'a str, window: &'a str) -> miette::Result<(SpanToken<'a>, &'a str)> {
    if let Some(rem) = window.strip_prefix("0b") {
        let pos = rem
            .char_indices()
            .find(|(_idx, ch)| !(('0'..='1').contains(ch) || *ch == '_'));
        let digits = if let Some((pos, _)) = pos {
            &rem[..pos]
        } else {
            rem
        };
        let fdigi = digits.chars().filter(|x| *x != '_').collect::<String>();
        let num = usize::from_str_radix(&fdigi, 2).unwrap();
        let (offset, len) = get_offset(input, digits).unwrap();
        Ok((
            SpanToken {
                span: (offset - 2, len + 2),
                token: Token::Number(num),
            },
            &window[(2 + digits.len())..],
        ))
    } else if let Some(rem) = window.strip_prefix("0o") {
        let pos = rem
            .char_indices()
            .find(|(_idx, ch)| !(('0'..='7').contains(ch) || *ch == '_'));
        let digits = if let Some((pos, _)) = pos {
            &rem[..pos]
        } else {
            rem
        };
        let fdigi = digits.chars().filter(|x| *x != '_').collect::<String>();
        let num = usize::from_str_radix(&fdigi, 8).unwrap();
        let (offset, len) = get_offset(input, digits).unwrap();
        Ok((
            SpanToken {
                span: (offset - 2, len + 2),
                token: Token::Number(num),
            },
            &window[(2 + digits.len())..],
        ))
    } else if let Some(rem) = window.strip_prefix("0x") {
        let pos = rem
            .char_indices()
            .find(|(_idx, ch)| !(ch.is_ascii_hexdigit() || *ch == '_'));
        let digits = if let Some((pos, _)) = pos {
            &rem[..pos]
        } else {
            rem
        };
        let fdigi = digits.chars().filter(|x| *x != '_').collect::<String>();
        let num = usize::from_str_radix(&fdigi, 16).unwrap();
        let (offset, len) = get_offset(input, digits).unwrap();
        Ok((
            SpanToken {
                span: (offset - 2, len + 2),
                token: Token::Number(num),
            },
            &window[(2 + digits.len())..],
        ))
    } else {
        let pos = window
            .char_indices()
            .find(|(_idx, ch)| !(ch.is_ascii_digit() || *ch == '_'));
        let digits = if let Some((pos, _)) = pos {
            &window[..pos]
        } else {
            window
        };
        let fdigi = digits.chars().filter(|x| *x != '_').collect::<String>();
        let Ok(num) = fdigi.parse::<usize>() else {
            return Err(BadDigit::new(window, digits).into());
        };
        let (offset, len) = get_offset(input, digits).unwrap();
        Ok((
            SpanToken {
                span: (offset, len),
                token: Token::Number(num),
            },
            &window[digits.len()..],
        ))
    }
}

#[cfg(test)]
mod test {
    use super::Lexer;

    #[test]
    fn one() {
        let input = r#"
            // chunker
            alias Chunky = u8;
            alias Bob123 = [u8; 100];
            template<X, Y> struct Billy {
                _bob: X,
                john: Y,
            }
        "#;
        let lexer = Lexer::lex(input).unwrap();
        for tok in lexer.tokens {
            println!("{tok:?} - '{}'", &input[tok.span.0..][..tok.span.1]);
        }
    }
}
