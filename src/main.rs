use std::{iter::Peekable, str::Chars};

fn main() {
    println!("Hello, world!");
    let source = "
    if + {comment} if /* comment */
    a + b
    ";
    let mut lexer = Lexer::new(source);

    loop {
        let token = lexer.read_next_token();
        println!("{:?}", token);
        if token.kind == Kind::Eof {
            break;
        }
    }
}

const KEYWORD: [&str; 28] = [
    "program",
    "var",
    "array",
    "of",
    "begin",
    "end",
    "if",
    "then",
    "else",
    "procedure",
    "return",
    "call",
    "while",
    "do",
    "not",
    "or",
    "div",
    "and",
    "char",
    "integer",
    "boolean",
    "read",
    "write",
    "readln",
    "writeln",
    "true",
    "false",
    "break",
];


#[derive(Debug, Clone, PartialEq)]
struct Token {
    kind: Kind,
    start: usize,
    end: usize,
    value: TokenValue,
}

#[derive(Debug, Clone, PartialEq)]
enum TokenValue {
    None,
    UInteger(u32),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Kind {
    Eof,
    Plus,
    Separator,
    Comment,
    Name,
    If,
}

struct Lexer<'a> {
    source: &'a str,
    chars: Peekable<Chars<'a>>,
    // chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: source.chars().peekable(),
        }
    }

    fn read_next_kind(&mut self) -> (Kind, TokenValue) {
        while let Some(c) = self.chars.next() {
            match c {
                // 分離子
                // 1文字で判定可能
                '\t' | '\n' | '\r' | ' ' => {
                    continue;
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
                    while let Some(c) = self.chars.next() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' => {
                                buf.push(c);
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
                _ => {}
            }
        }
        (Kind::Eof, TokenValue::None)
    }

    fn read_next_token(&mut self) -> Token {
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

    fn offset(&self) -> usize {
        // let len = self.source.len();
        // let pos = self.chars.;
        // self.source.len() - self.chars.count()
        // unimplemented!("offset");
        999
    }

    fn match_keyword(&self, ident: &str) -> Kind {
        if ident.len() == 1 || ident.len() > 10 {
            return Kind::Name;
        }
        match ident {
            "if" => Kind::If,
            _ => Kind::Name,
        }
    }
}
