use std::{iter::Peekable, str::Chars};

fn main() {
    println!("Hello, world!");
    let source = "
    if + {comment} if /* comment */
    a + b
    1+1
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
    // わかりやすいようにSeparatorも含めてる なくてもいいのか？
    Separator,
    Comment,
    Name,
    UnsignedInteger,
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
            // chars: source.chars(),
            chars: source.chars().peekable(),
        }
    }

    fn read_next_kind(&mut self) -> (Kind, TokenValue) {
        while let Some(c) = self.chars.next() {
            match c {
                // 分離子
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
                    return (Kind::UnsignedInteger, TokenValue::UInteger(unsigned_int))
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
        // self.chars.clone().count()の計算量を調べた方がいいかもしれない
        // self.source.len()は fat pointerによりO(1)だが，後者はO(n)の可能性あり
        self.source.len() - self.chars.clone().count()
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
