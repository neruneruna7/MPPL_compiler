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
    UInteger(u32),
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
        while let Some(c) = self.chars.next() {
            match c {
                // 分離子 注釈以外
                // 1文字で判定可能
                '\t' | '\n' | '\r' | ' ' => {
                    while let Some(cc) = self.chars.peek() {
                        match cc {
                            '\t' | '\n' | '\r' | ' ' => {
                                let _cc = self.chars.next().unwrap();
                            }
                            _ => {
                                return (Kind::Separator, TokenValue::None);
                            }
                        }
                    }

                    // 最後のEOFの前に，Separatorを検出するか否かが以下のreturn文で決まる
                    // あれば検出 なければ検出しない
                    // continue;
                    // return (Kind::Separator, TokenValue::None);
                }
                '+' => return (Kind::Plus, TokenValue::None),
                // 注釈（コメント）
                // {または/から始まる
                '{' => {
                    let mut buf = String::new();
                    while let Some(c) = self.chars.next() {
                        match c {
                            '}' => break,
                            _ => {
                                buf.push(c);
                            }
                        }
                    }
                    return (Kind::Comment, TokenValue::String(buf));
                }
                '/' => {
                    if self.chars.next() != Some('*') {
                        eprintln!("expected * after /");
                    }
                    let mut buf = String::new();
                    while let Some(c) = self.chars.next() {
                        match c {
                            '*' => {
                                if self.chars.next() == Some('/') {
                                    return (Kind::Comment, TokenValue::String(buf));
                                }
                            }
                            _ => {
                                buf.push(c);
                            }
                        }
                    }
                }
                // キーワードまたは名前 この2つは英字から始まる
                'a'..='z' | 'A'..='Z' => {
                    let mut buf = String::from(c);
                    while let Some(c) = self.chars.peek() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                let cc = self.chars.next().unwrap();
                                buf.push(cc);
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    // 名前かキーワードかを判別
                    let kind = self.match_keyword(&buf);
                    // 名前なら
                    match kind {
                        Kind::Name => return (kind, TokenValue::String(buf)),
                        _ => return (kind, TokenValue::None),
                    }
                }
                // 符号なし整数
                '0'..='9' => {
                    let mut buf = String::from(c);
                    while let Some(c) = self.chars.peek() {
                        match c {
                            '0'..='9' => {
                                let cc = self.chars.next().unwrap();
                                buf.push(cc);
                            }
                            _ => {
                                break;
                            }
                        }
                    }
                    let unsigned_int = buf.parse().unwrap();
                    return (Kind::UnsignedInteger, TokenValue::UInteger(unsigned_int));
                }
                _ => {
                    let mut buf = String::from(c);
                    let kind = self.match_symbol(&buf);
                    match kind {
                        // 名前だったら記号ではない
                        // 他の選択肢はすでにここより上の条件に引っかかっているため
                        Kind::Name => break,
                        _ => {}
                    }

                    if let Some(cc) = self.chars.peek() {
                        match cc {
                            '=' | '>' | '<' => {
                                let ccc = self.chars.next().unwrap();
                                buf.push(ccc);
                            }
                            _ => {}
                        }
                    }

                    let kind = self.match_symbol(&buf);
                    return (kind, TokenValue::None);
                }
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
