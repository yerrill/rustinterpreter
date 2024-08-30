use std::cmp::min;
use std::fmt::{format, Debug};

#[derive(Debug, PartialEq)]
enum Token {
    EOF,
    ILLEGAL { literal: String },

    IDENT { literal: String },
    STRING { literal: String },
    INT { literal: i64 },
    FLOAT { literal: f64 },

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    FSLASH,
    BSLASH,
    MOD,

    LT,
    GT,
    LTEQ,
    GTEQ,
    EQ,
    NOTEQ,

    COMMA,
    LINEFEED,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,

    FUNCTION,
    LET,
    TYPE,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

impl Token {
    fn keywords(literal: &str) -> Option<Token> {
        match literal {
            "fn" => Some(Token::FUNCTION),
            "let" => Some(Token::LET),
            "type" => Some(Token::TYPE),
            "true" => Some(Token::TRUE),
            "false" => Some(Token::FALSE),
            "if" => Some(Token::IF),
            "else" => Some(Token::ELSE),
            "return" => Some(Token::RETURN),
            _ => None,
        }
    }

    fn to_string(token: &Token) -> String {
        match token {
            Token::EOF => String::from("EOF"),
            Token::ILLEGAL { literal } => format!("ILLEGAL({literal})"),
            Token::IDENT { literal } => format!("IDENT({literal})"),
            Token::STRING { literal } => format!("\"{literal})\""),
            Token::INT { literal } => format!("{literal}"),
            Token::FLOAT { literal } => format!("{literal}"),
            Token::ASSIGN => String::from("="),
            Token::PLUS => String::from("+"),
            Token::MINUS => String::from("-"),
            Token::BANG => String::from("!"),
            Token::ASTERISK => String::from("*"),
            Token::FSLASH => String::from("/"),
            Token::BSLASH => String::from("\\"),
            Token::MOD => String::from("%"),
            Token::LT => String::from("<"),
            Token::GT => String::from(">"),
            Token::LTEQ => String::from("<="),
            Token::GTEQ => String::from(">="),
            Token::EQ => String::from("=="),
            Token::NOTEQ => String::from("!="),
            Token::COMMA => String::from(","),
            Token::LINEFEED => String::from("\n"),
            Token::LPAREN => String::from("("),
            Token::RPAREN => String::from(")"),
            Token::LBRACE => String::from("{"),
            Token::RBRACE => String::from("}"),
            Token::LBRACKET => String::from("["),
            Token::RBRACKET => String::from("]"),
            Token::FUNCTION => String::from("func"),
            Token::LET => String::from("let"),
            Token::TYPE => String::from("type"),
            Token::TRUE => String::from("true"),
            Token::FALSE => String::from("false"),
            Token::IF => String::from("if"),
            Token::ELSE => String::from("else"),
            Token::RETURN => String::from("return"),
        }
    }
}

#[derive(Debug)]
struct Lexer {
    input: Vec<char>,
    cursor: usize,
}

impl Lexer {
    fn new(input_string: &str) -> Lexer {
        Lexer {
            input: input_string.chars().collect(),
            cursor: 0,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let cursor = self.cursor;

        if cursor >= self.input.len() {
            None
        } else {
            self.cursor += 1;
            Some(self.input[cursor])
        }
    }

    fn peek_char(&self) -> Option<char> {
        if self.cursor < self.input.len() {
            Some(self.input[self.cursor])
        } else {
            None
        }
    }

    fn eat_whitespace(&mut self) {
        let iter = self.input.iter().enumerate().skip(self.cursor);

        let mut new_cursor = self.cursor;

        for (ind, ch) in iter {
            if *ch == ' ' || *ch == '\t' || *ch == '\r' {
                new_cursor = ind + 1;
            } else {
                break;
            }
        }

        self.cursor = new_cursor;
    }

    // Get next input from the boundaries
    fn next_token(&mut self) -> Option<Result<Token, String>> {
        self.eat_whitespace();

        let char: char = match self.read_char() {
            Some(c) => c,
            None => return None,
        };

        let new_token: Option<Result<Token, String>> = match char {
            '=' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Some(Ok(Token::EQ))
                } else {
                    Some(Ok(Token::ASSIGN))
                }
            }
            '+' => Some(Ok(Token::PLUS)),
            '-' => Some(Ok(Token::MINUS)),
            '!' => {
                if self.peek_char() == Some('=') {
                    Some(Ok(Token::NOTEQ))
                } else {
                    Some(Ok(Token::BANG))
                }
            }
            '*' => Some(Ok(Token::ASTERISK)),
            '/' => Some(Ok(Token::FSLASH)),
            '\\' => {
                // Idk what I'm doing with this yet. Escape character? Lambda? Skelletons? Communism? Piss?
                Some(Ok(Token::BSLASH))
            }
            '%' => Some(Ok(Token::MOD)),
            '<' => {
                // Needs <=
                Some(Ok(Token::LT))
            }
            '>' => {
                // Needs >=
                Some(Ok(Token::GT))
            }
            ',' => Some(Ok(Token::COMMA)),
            '\n' => Some(Ok(Token::LINEFEED)),
            '\"' => {
                // Needs full string scanning
                Some(Ok(Token::STRING {
                    literal: String::from("Placeholder"),
                }))
            }
            '(' => Some(Ok(Token::LPAREN)),
            ')' => Some(Ok(Token::RPAREN)),
            '{' => Some(Ok(Token::LBRACE)),
            '}' => Some(Ok(Token::RBRACE)),
            '[' => Some(Ok(Token::LBRACKET)),
            ']' => Some(Ok(Token::RBRACKET)),

            other => Some(Err(other.to_string())),
        };

        new_token
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, String>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

fn main() {
    let l = Lexer::new("< > []{ } + = - == != === =!== /\\,* \n =letreturn let return ");

    for tok in l {
        println!("{:?}", tok);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    fn run_test(lexer_input: String, expected_output: Vec<Token>) {
        let test_lexer: Lexer = Lexer::new(&lexer_input);

        for (index, token) in test_lexer.enumerate() {
            match token {
                Ok(t) => assert_eq!(t, expected_output[index]),
                Err(e) => panic!("{}", e),
            };

        }
    }

    #[test]
    fn single_char_basic() {
        let s: String = String::from("< > [ ] { } ( ) = + - * / \\ % ! , \n");
        let result: Vec<Token> = vec![
            Token::LT, Token::GT, Token::LBRACKET, Token::RBRACKET, 
            Token::LBRACE, Token::RBRACE, Token::LPAREN, Token::RPAREN,
            Token::ASSIGN, Token::PLUS, Token::MINUS, Token::ASTERISK, 
            Token::FSLASH, Token::BSLASH, Token::MOD, Token::BANG,
            Token::COMMA, Token::LINEFEED];
        
        run_test(s, result);
    }
}