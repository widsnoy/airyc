use std::ops::Range;

use logos::Logos;

#[derive(Debug, Default, Clone, PartialEq)]
pub enum LexerErrorKind {
    InvalidInteger,
    InvalidFloat,
    #[default]
    Unknown,
}

#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[error("Lexer error: {kind:?} at {span:?}: {text}")]
pub struct LexerError {
    pub kind: LexerErrorKind,
    pub text: String,
    pub span: Range<usize>,
}

/// SyntaxKind
#[derive(Logos, Debug, PartialEq, Clone)]
#[logos(error = LexerErrorKind)]
pub enum Token {
    // Trivia
    #[regex(r"[ \t]+")]
    Whitespace,
    #[regex(r"(\r\n|\n|\r)+")]
    Newline,
    #[regex(r"//[^\n]*")]
    LineComment,
    #[regex(r"/\*([^*]|\*[^/])*\*/")]
    BlockComment,
    // Keywords
    #[token("const")]
    Const,
    #[token("int")]
    Int,
    #[token("float")]
    Float,
    #[token("void")]
    Void,
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("break")]
    Break,
    #[token("continue")]
    Continue,
    #[token("return")]
    Return,
    #[token("struct")]
    Struct,
    #[token("impl")]
    Impl,

    // Operators and punctuation
    #[token("=")]
    Eq,
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("*")]
    Star,
    #[token(".")]
    Dot,
    #[token("->")]
    Arrow,

    // Arithmetic operators
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,

    // Comparison operators
    #[token("==")]
    EqEq,
    #[token("!=")]
    Ne,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Le,
    #[token(">=")]
    Ge,

    // Logical operators
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,

    /// ref address
    #[token("&")]
    Amp,

    // Literals
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),
    #[regex(r"0[xX][0-9a-fA-F]+", |lex| i32::from_str_radix(&lex.slice()[2..], 16).map_err(|_| LexerErrorKind::InvalidInteger), priority = 3)]
    #[regex(r"0[0-7]*", |lex| i32::from_str_radix(lex.slice(), 8).map_err(|_| LexerErrorKind::InvalidInteger), priority = 3)]
    #[regex(r"[1-9][0-9]*", |lex| lex.slice().parse().map_err(|_| LexerErrorKind::InvalidInteger), priority = 3)]
    IntLiteral(i32),

    #[regex(r"(\d+(\.\d*)?|\.\d+)([eE][+-]?\d+)?", |lex| lex.slice().parse().map_err(|_| LexerErrorKind::InvalidFloat), priority = 2)]
    FloatLiteral(f32),

    // EOF
    EOF,
}

/// Lexer
pub struct Lexer {
    tokens: Vec<(Token, Range<usize>)>,
}

impl Lexer {
    /// Create a new lexer from the given text
    pub fn new(text: &str) -> Result<Self, LexerError> {
        let mut tokens = Vec::new();
        let mut lexer = Token::lexer(text);

        while let Some(result) = lexer.next() {
            let span = lexer.span();
            match result {
                Ok(token) => tokens.push((token, span)),
                Err(kind) => {
                    return Err(LexerError {
                        kind,
                        text: lexer.slice().to_string(),
                        span,
                    });
                }
            }
        }

        tokens.reverse();

        Ok(Self { tokens })
    }

    /// Get the next token
    pub fn next(&mut self) -> Option<(Token, Range<usize>)> {
        self.tokens.pop()
    }

    /// Peek at the next token without consuming it
    pub fn peak(&self) -> Token {
        match self.tokens.last() {
            Some((token, _)) => token.clone(),
            None => Token::EOF,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lexer_basic() {
        let source = "const int x = 42;";
        let mut lexer = Lexer::new(source).unwrap();

        let expected_tokens = vec![
            (Token::Const, 0..5),
            (Token::Whitespace, 5..6),
            (Token::Int, 6..9),
            (Token::Whitespace, 9..10),
            (Token::Ident("x".to_string()), 10..11),
            (Token::Whitespace, 11..12),
            (Token::Eq, 12..13),
            (Token::Whitespace, 13..14),
            (Token::IntLiteral(42), 14..16),
            (Token::Semicolon, 16..17),
        ];

        for expected in expected_tokens {
            let token = lexer.next().unwrap();
            assert_eq!(token, expected);
        }

        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_star_token() {
        let source = "int* ptr;";
        let mut lexer = Lexer::new(source).unwrap();
        let expected_tokens = vec![
            (Token::Int, 0..3),
            (Token::Star, 3..4),
            (Token::Whitespace, 4..5),
            (Token::Ident("ptr".to_string()), 5..8),
            (Token::Semicolon, 8..9),
        ];
        for expected in expected_tokens {
            let token = lexer.next().unwrap();
            assert_eq!(token, expected);
        }
        assert!(lexer.next().is_none());
    }
}
