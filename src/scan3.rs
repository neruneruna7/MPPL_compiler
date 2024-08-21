use std::{iter::Peekable, str::Chars};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) struct Token {
    pub(crate) kind: Kind,
    pub(crate) start: usize,
    pub(crate) end: usize,
    pub(crate) value: TokenValue,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub(crate) enum TokenValue {
    None,
    Integer(u32),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Kind {
    Eof,
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

    pub(crate) fn read_next_token(&mut self) -> Token {
        while let Some(c) = self.chars.peek() {
            // EBNFのprogramに該当
            match c {
                // 分離子
                ' ' | '\t' | '\n' | '{' | '/' => {
                    self.comment();
                }
                // 字句
                _ => {
                    let start = self.offset();
                    // let (kind, value) = self.read_next_kind();
                    let (kind, value) =  self.token();
                    let end = self.offset();

                    return Token {
                        kind,
                        start,
                        end,
                        value,
                    };
                }
            }
        }
        let start = self.offset();
        let end = self.offset();

        Token {
            kind: Kind::Eof,
            start,
            end,
            value: TokenValue::None,
        }
    }

    pub(crate) fn offset(&self) -> usize {
        // self.chars.clone().count()の計算量を調べた方がいいかもしれない
        // self.source.len()は fat pointerによりO(1)だが，後者はO(n)の可能性あり

        // イテレータを消費し，Noneを返すまでの要素数を返す
        // ので，count()の計算量はO(n)になると思う
        // ややコストが高めかもしれない
        self.source.len() - self.chars.clone().count()
    }

    fn comment(&mut self) {
        // EBNFのcomment，注釈に該当
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

    fn token(&mut self) -> (Kind, TokenValue) {
        // EBNFのtoken，字句に該当
        
        // panicしないことを保証したうえでunwrapしている
        // 呼び出しのpeekで存在を確認しているのでunwrapでpanicは起きない 
        let c = self.chars.peek().unwrap(); 
        match c {
            'a'..='z' | 'A'..='Z' => {
                return self.name_keyword();
            }
            '0'..='9' => {
                return self.unsigned_integer();
            }
            _ => {
                return self.symbol();
            }
        }
    }

    fn name_keyword(&mut self) -> (Kind, TokenValue) {
        let mut name = String::new();
        // 呼び出しのpeekで存在を確認しているのでunwrapでpanicは起きない
        name.push(self.chars.next().unwrap());

        while let Some(c) = self.chars.peek() {
            match c {
                'a'..='z' | 'A'..='Z' | '0'..='9' => {
                    name.push(self.chars.next().unwrap());
                }
                _ => {
                    break;
                }
            }
        }
        let kind = self.match_keyword(&name);
        (kind, TokenValue::String(name))
    }

    fn unsigned_integer(&mut self) -> (Kind, TokenValue) {
        let mut integer = String::new();
        integer.push(self.chars.next().unwrap());

        while let Some(c) = self.chars.peek() {
            match c {
                '0'..='9' => {
                    integer.push(self.chars.next().unwrap());
                }
                _ => {
                    break;
                }
            }
        }
        return (
            Kind::UnsignedInteger,
            TokenValue::Integer(integer.parse().unwrap()),
        );
    }

    fn symbol(&mut self) -> (Kind, TokenValue) {
        let c = self.chars.next().unwrap();
        let mut buf = String::from(c);
        while let Some(c) = self.chars.peek() {
            let cc = String::from(*c);
            if self.match_symbol(&cc) == Kind::Name {
                break;
            }
            buf.push(self.chars.next().unwrap());
        }

        let kind = self.match_symbol(&buf);
        (kind, TokenValue::None)
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
