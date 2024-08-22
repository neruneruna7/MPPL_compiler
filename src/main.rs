use prac_compiler::scan::scan3;

mod parser {
    mod parser1 {
        use crate::scan3::{self, Kind, Lexer, Token};

        pub struct Parser<'a> {
            lexer: Lexer<'a>,
            lookahead: Option<Token>,
        }

        impl<'a> Parser<'a> {
            pub fn new(mut lexer: Lexer<'a>) -> Self {
                let init_token = lexer.read_next_token();
                Self {
                    lexer,
                    lookahead: Some(init_token),
                    // cur_token,
                    // prev_token_end
                }
            }

            fn match_token(&mut self, kind: scan3::Kind) {
                match self.lookahead {
                    Some(ref l) => {
                        if l.kind == kind {
                            self.lookahead = Some(self.lexer.read_next_token());
                            return;
                        }
                    }
                    None => {}
                }
                panic!(
                    "syntax error, expect {:?}; found {:?}",
                    kind, &self.lookahead
                );
            }

            // "program" "名前" ";" ブロック "."
            pub fn parse_program(&mut self) {
                // マクロ構文のprogramに該当
                self.match_token(Kind::Program);
                self.match_token(Kind::Name);
                self.match_token(Kind::Semicolon);
                // self.block();
                self.match_token(Kind::Dot);
            }

            // { 変数宣言部 | 副プログラム宣言 } 複合文
            fn block(&mut self) {}
        }
    }
}

fn main() {
    println!("Hello, world!");
    let source = "
    if + {comment} if /* comment */
    a + b
    1+1
    if true then
        1 + 1
    else
        4 - 3
    ";

    let source = "
    if true then
    if if if if if if
    c c c c c c c c 
    { this is a comment }
        1 + 1
    else
        i := 5
    ";

    let source = "
    program sample; .";

    let mut lexer = scan3::Lexer::new(source);
    let tokens = lexer.analyze();

    // for i in tokens.iter() {
    //     println!("{:?}", i);
    // }

    // 字句の出現数をカウント
    let mut count = std::collections::HashMap::new();
    for token in tokens.into_iter() {
        let token = (token.kind, token.value);
        let entry = count.entry(token).or_insert(0);
        *entry += 1;
    }

    for (token, count) in count.iter() {
        println!("{:?} : {}", token, count);
    }
}
