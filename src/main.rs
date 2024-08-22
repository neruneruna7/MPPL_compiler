use prac_compiler::scan::scan3;

pub mod parser {
    pub mod parser1 {
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
                    None => {
                    }
                }

                let sliced_source = if let Some(ref l) = self.lookahead {
                    &self.lexer.source[..l.start]
                } else {
                    &self.lexer.source
                };

                panic!(
                    "syntax error source code:\n\n {} \n\n expect {:?} but found {:?}",
                    sliced_source, kind, &self.lookahead
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
    let source = "
    program sample; if .";

    let mut lexer = scan3::Lexer::new(source);
    let mut parser = parser::parser1::Parser::new(lexer);
    parser.parse_program();

    println!("parsing OK \n{}", source);
}
