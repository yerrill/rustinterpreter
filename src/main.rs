use std::fmt::Debug;

#[derive(Debug, PartialEq)]
enum Token {
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
    fn keyword_to_token(literal: &str) -> Option<Token> {
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
            Token::FUNCTION => String::from("fn"),
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

    fn is_whitespace(ch: char) -> bool {
        if ch == ' ' || ch == '\t' || ch == '\r' {
            true
        } else {
            false
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
            if Lexer::is_whitespace(*ch) {
                println!("{}", ch);
                new_cursor = ind + 1;
            } else {
                break;
            }
        }

        self.cursor = new_cursor;
    }

    // Seek in input from cursor until predicate returns true, return ending bound
    fn seek_predicate(&self, p: fn(char) -> bool) -> usize {
        let mut end = self.cursor;
        let iter = self.input.iter().enumerate().skip(self.cursor);
        let mut found_predicate: bool = false;

        for (ind, ch) in iter {
            end = ind;
            found_predicate = p(*ch);

            if found_predicate {
                break;
            }
        }

        if !found_predicate {
            end += 1;
        }

        end
    }

    fn read_slice(&mut self, end: usize) -> String {
        assert!(end <= self.input.len(), "read_slice {:?}", dbg!(self, end));
        assert!(end > 0, "read_slice {:?}", dbg!(self, end));

        let start = self.cursor;
        self.cursor = end;

        dbg!(&self, start, end);

        self.input[start..end].iter().collect()
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
                    self.read_char();
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
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Some(Ok(Token::LTEQ))
                } else {
                    Some(Ok(Token::LT))
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.read_char();
                    Some(Ok(Token::GTEQ))
                } else {
                    Some(Ok(Token::GT))
                }
            }
            ',' => Some(Ok(Token::COMMA)),
            '\n' => Some(Ok(Token::LINEFEED)),
            '\"' => {
                // Needs full string scanning
                let end_bound = self.seek_predicate(|ch| ch == '\"');
                let literal = self.read_slice(end_bound);
                self.cursor += 1;
                Some(Ok(Token::STRING { literal: literal }))
            }
            '(' => Some(Ok(Token::LPAREN)),
            ')' => Some(Ok(Token::RPAREN)),
            '{' => Some(Ok(Token::LBRACE)),
            '}' => Some(Ok(Token::RBRACE)),
            '[' => Some(Ok(Token::LBRACKET)),
            ']' => Some(Ok(Token::RBRACKET)),

            other => {
                if char.is_alphabetic() { // Identifier or Keyword
                    let end_char = self.seek_predicate(|ch| Lexer::is_whitespace(ch) || ch == '\n');
                    self.cursor -= 1;
                    let literal = self.read_slice(end_char);

                    match Token::keyword_to_token(&literal) {
                        None => Some(Ok(Token::IDENT { literal: literal })),
                        Some(token) => {
                            Some(Ok(token))
                        }
                    }

                } else if char.is_numeric() || char == '.' {
                    self.cursor -= 1;
                    let end_char = self.seek_predicate(|ch| Lexer::is_whitespace(ch) || ch == '\n');
                    let literal = self.read_slice(end_char);
                    
                    if let Ok(int) = literal.parse::<i64>() {
                        Some(Ok(Token::INT { literal: int }))
                    } else if let Ok(flt) = literal.parse::<f64>() {
                        Some(Ok(Token::FLOAT { literal: flt }))
                    } else {
                        Some(Err(literal))
                    }
                } else {
                    Some(Err(other.to_string()))
                }
            },
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
mod lexer_tests {
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

    fn run_test_with_err(lexer_input: String, expected_output: Vec<Result<Token, String>>) {
        let test_lexer: Lexer = Lexer::new(&lexer_input);

        for (index, token) in test_lexer.enumerate() {
            assert_eq!(token, expected_output[index]);
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

    #[test]
    fn two_char_tokens() {
        let s: String = String::from("<= >= == != < = > !");
        let result: Vec<Token> = vec![
            Token::LTEQ, Token::GTEQ, Token::EQ, Token::NOTEQ, 
            Token::LT, Token::ASSIGN, Token::GT, Token::BANG];

        run_test(s, result);
    }

    #[test]
    fn identifiers() {
        let s: String = String::from("test identifier < blah\nblah");
        let result: Vec<Token> = vec![
            Token::IDENT { literal: "test".to_string() },
            Token::IDENT { literal: "identifier".to_string() },
            Token::LT,
            Token::IDENT { literal: "blah".to_string() },
            Token::LINEFEED,
            Token::IDENT { literal: "blah".to_string() }];

        run_test(s, result);
    }

    #[test]
    fn keywords() {
        let s: String = String::from("\nfn\nlet type true false if else\nreturn");
        let result: Vec<Token> = vec![
            Token::LINEFEED, Token::FUNCTION, Token::LINEFEED,
            Token::LET, Token::TYPE, Token::TRUE, Token::FALSE,
            Token::IF, Token::ELSE, Token::LINEFEED, Token::RETURN];

        run_test(s, result);
    }

    #[test]
    fn identifiers_and_keywords() {
        let s: String = String::from("test fn type identifier let blahtrueblah");
        let result: Vec<Token> = vec![
            Token::IDENT { literal: "test".to_string() },
            Token::FUNCTION, Token::TYPE,
            Token::IDENT { literal: "identifier".to_string() },
            Token::LET,
            Token::IDENT { literal: "blahtrueblah".to_string() }];

        run_test(s, result);
    }

    #[test]
    fn strings() {
        let s: String = String::from("\"test test\" ident fn \"str\" \"str fn type true false");
        let result: Vec<Token> = vec![
            Token::STRING { literal: "test test".to_string() },
            Token::IDENT { literal: "ident".to_string() },
            Token::FUNCTION,
            Token::STRING { literal: "str".to_string() },
            Token::STRING { literal: "str fn type true false".to_string() }];

        run_test(s, result);
    }

    #[test]
    fn integers() {
        let s: String = String::from("test fn type identifier let blahtrueblah");
        let result: Vec<Token> = vec![
            Token::IDENT { literal: "test".to_string() },
            Token::FUNCTION, Token::TYPE,
            Token::IDENT { literal: "identifier".to_string() },
            Token::LET,
            Token::IDENT { literal: "blahtrueblah".to_string() }];

        run_test(s, result);
    }

    #[test]
    fn numbers() {
        let s: String = String::from("1 2 3.14 123.123.32 .12");
        let result: Vec<Result<Token, String>> = vec![
            Ok(Token::INT { literal: 1 }),
            Ok(Token::INT { literal: 2 }),
            Ok(Token::FLOAT { literal: 3.14 }),
            Err("123.123.32".to_string()),
            Ok(Token::FLOAT { literal: 0.12 })];

        run_test_with_err(s, result);
    }
}