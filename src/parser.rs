//! This module exists to define the ast as parsed
//!
//! This defines the AST, and the items we need to be able to parse it.
use std::iter::Peekable;
use std::str::Chars;

/// A token produced by our lexer
#[derive(Debug, PartialEq)]
enum Token {
    /// The character `(`
    OpenParens,
    /// The character `)`
    CloseParens,
    /// The keyword `do`
    Do,
    /// the keyword `if`
    If,
    /// A sequence of tokens we can interpret as a string
    Str(String),
    /// A name not assigned to some keyword
    Name(String),
    /// An integer litteral
    I64(i64),
}

/// The lexerr takes in a source string, and spits out tokens
struct Lexer<'a> {
    /// The source stream of characters to lex
    chars: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Lexer {
            chars: source.chars().peekable(),
        }
    }

    fn get_i64(&mut self, starter: char) -> i64 {
        let mut acc = String::new();
        acc.push(starter);
        while let Some(c) = self.chars.peek() {
            if !c.is_numeric() {
                break;
            }
            acc.push(*c);
            self.next();
        }
        acc.parse::<i64>().unwrap()
    }

    fn string(&mut self) -> String {
        let mut acc = String::new();
        while let Some(c) = self.chars.next() {
            if c == '"' {
                break;
            }
            acc.push(c);
        }
        acc
    }

    /// This allows us to parse a contiguous name, basically
    /// a sequence of characters not interrupted by whitespace or other tokens
    fn word(&mut self, starter: char) -> String {
        let mut acc = String::new();
        acc.push(starter);
        while let Some(c) = self.chars.peek() {
            let should_break = match c {
                '(' => true,
                ')' => true,
                c if c.is_whitespace() => true,
                _ => false,
            };
            if should_break {
                break;
            }
            // We can unwrap without remorse since we peeked
            acc.push(*c);
            self.next();
        }
        acc
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.chars.next() {
            match c {
                '(' => return Some(Token::OpenParens),
                ')' => return Some(Token::CloseParens),
                '"' => return Some(Token::Str(self.string())),
                n if n.is_numeric() => return Some(Token::I64(self.get_i64(n))),
                w if w.is_whitespace() => {}
                _ => {
                    let ident = self.word(c);
                    let out = match ident.as_ref() {
                        "do" => Token::Do,
                        "if" => Token::If,
                        _ => Token::Name(ident),
                    };
                    return Some(out);
                }
            }
        }
        None
    }
}

/// Represents a litteral that an expression evaluates to
enum Litt {
    /// An integer
    I64(i64),
    /// A string
    Str(String),
    /// The magical null element
    Nil,
}

/// Represents some kind of expression that can be evaluated to a litteral
enum Expr {
    /// A list containing multiple expressions, e.g. `(f 1 2 (+ 3 4))`
    List(Vec<Expr>),
    /// A reference to same name, e.g. `f`
    Name(String),
    // Then we have all the keywords
    Do,
    If,
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lexer_works_for_string_litterals() {
        let text = "\"(foo)\n3\"";
        let tokens: Vec<Token> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Token::Str("(foo)\n3".into())])
    }

    #[test]
    fn lexer_works_for_names() {
        let text = "(+ a b)";
        let tokens: Vec<Token> = Lexer::new(text).collect();
        assert_eq!(
            tokens,
            vec![
                Token::OpenParens,
                Token::Name("+".into()),
                Token::Name("a".into()),
                Token::Name("b".into()),
                Token::CloseParens
            ]
        );
    }

    #[test]
    fn lexer_works_for_keywords() {
        let text = "do if";
        let tokens: Vec<Token> = Lexer::new(text).collect();
        assert_eq!(tokens, vec![Token::Do, Token::If]);
    }
}
