use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) value: TokenValue,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum TokenValue {
    None,
    Integer(u32),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Kind {
    Eof,
    // わかりやすいようにSeparatorも含めてる なくてもいいのか？
    Separator,
    Comment,
    Name,
    UnsignedInteger,
    // 以下キーワード
    Program,
    Var,
    Array,
    Of,
    Begin,
    End,
    If,
    Then,
    Else,
    Procedure,
    Return,
    Call,
    While,
    DO,
    Not,
    Or,
    Div,
    And,
    Char,
    Integer,
    Boolean,
    Read,
    Write,
    Readln,
    Writeln,
    True,
    False,
    Break,
    // 以下記号
    Plus,
    Minus,
    Star,
    Equal,
    NotEq,
    Less,
    LessEq,
    Great,
    GreatEq,
    LParen,
    RParen,
    LBracket,
    RBracket,
    Assign,
    Dot,
    Comma,
    Colon,
    Semicolon,
}

pub(crate) struct Lexer<'a> {
    pub(crate) source: &'a str,
    pub(crate) chars: Peekable<Chars<'a>>,
    // chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(source: &'a str) -> Self {
        Self {
            source,
            // chars: source.chars(),
            chars: source.chars().peekable(),
        }
    }

    pub(crate) fn analyze(&mut self) -> Vec<Token> {
        let mut token_vec = Vec::new();
        loop {
            let token = self.read_next_token();
            if token.kind == Kind::Eof {
                token_vec.push(token);
                break;
            } else {
                token_vec.push(token);
            }
        }
        token_vec
    }

    pub(crate) fn read_next_kind(&mut self) -> (Kind, TokenValue) {
        while let Some(c) = self.chars.peek() {
            match *c {
                // 分離子
                ' ' | '\t' | '\n' | '{' | '/' => {
                    self.comment();
                }
                _ => {}
            }
        }
        (Kind::Eof, TokenValue::None)
    }

    pub(crate) fn read_next_token(&mut self) -> Token {
        let start = self.offset();
        let (kind, value) = self.read_next_kind();
        let end = self.offset();
        Token {
            kind,
            start,
            end,
            value,
        }
    }

    pub(crate) fn offset(&self) -> usize {
        // self.chars.clone().count()の計算量を調べた方がいいかもしれない
        // self.source.len()は fat pointerによりO(1)だが，後者はO(n)の可能性あり
        self.source.len() - self.chars.clone().count()
    }

    fn comment(&mut self) {
        if let Some(c) = self.chars.next() {
            match c {
                '{' => {
                    self.comment_brace();
                }
                '/' => {
                    self.comment_slashstar();
                }
                _ => {}
            }
        }
    }
    fn comment_brace(&mut self) {
        let c = self.chars.next().unwrap();
        while let Some(c) = self.chars.next() {
            if c == '}' {
                break;
            }
        }
    }

    fn comment_slashstar(&mut self) {
        enum State {
            Slash,
            Star,
            Other,
        }
        let mut state = State::Slash;
        while let Some(c) = self.chars.next() {
            match state {
                State::Slash => {
                    if c == '*' {
                        state = State::Star;
                    }
                }
                State::Star => {
                    if c == '/' {
                        break;
                    } else if c != '*' {
                        state = State::Other;
                    }
                }
                State::Other => {
                    if c == '*' {
                        state = State::Star;
                    }
                }
            }
        }
    }

    pub(crate) fn match_keyword(&self, ident: &str) -> Kind {
        if ident.len() == 1 || ident.len() > 10 {
            return Kind::Name;
        }
        match ident {
            "program" => Kind::Program,
            "var" => Kind::Var,
            "array" => Kind::Array,
            "of" => Kind::Of,
            "begin" => Kind::Begin,
            "end" => Kind::End,
            "if" => Kind::If,
            "then" => Kind::Then,
            "else" => Kind::Else,
            "procedure" => Kind::Procedure,
            "return" => Kind::Return,
            "call" => Kind::Call,
            "while" => Kind::While,
            "do" => Kind::DO,
            "not" => Kind::Not,
            "or" => Kind::Or,
            "div" => Kind::Div,
            "and" => Kind::And,
            "char" => Kind::Char,
            "integer" => Kind::Integer,
            "boolean" => Kind::Boolean,
            "read" => Kind::Read,
            "write" => Kind::Write,
            "readln" => Kind::Readln,
            "writeln" => Kind::Writeln,
            "true" => Kind::True,
            "false" => Kind::False,
            "break" => Kind::Break,
            _ => Kind::Name,
        }
    }

    pub(crate) fn match_symbol(&self, symbol: &str) -> Kind {
        match symbol {
            "+" => Kind::Plus,
            "-" => Kind::Minus,
            "*" => Kind::Star,
            "=" => Kind::Equal,
            "<>" => Kind::NotEq,
            "<" => Kind::Less,
            "<=" => Kind::LessEq,
            ">" => Kind::Great,
            ">=" => Kind::GreatEq,
            "(" => Kind::LParen,
            ")" => Kind::RParen,
            "[" => Kind::LBracket,
            "]" => Kind::RBracket,
            ":=" => Kind::Assign,
            "." => Kind::Dot,
            "," => Kind::Comma,
            ":" => Kind::Colon,
            ";" => Kind::Semicolon,
            _ => Kind::Name,
        }
    }
}
