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

        self.syntax_error();
    }

    fn syntax_error(&self) {
        let sliced_source = if let Some(ref l) = self.lookahead {
            &self.lexer.source[..l.start]
        } else {
            &self.lexer.source
        };

        panic!(
            "syntax error source code:\n\n {} \n\n expect {:?} but found {:?}",
            sliced_source,
            Kind::Eof,
            &self.lookahead
        );
    }

    /// パースの開始
    /// "program" "名前" ";" ブロック "."
    pub fn parse_program(&mut self) {
        // マクロ構文のprogramに該当
        self.match_token(Kind::Program);
        self.match_token(Kind::Name);
        self.match_token(Kind::Semicolon);
        // self.block();
        self.match_token(Kind::Dot);
    }

    /// { 変数宣言部 | 副プログラム宣言 } 複合文
    fn block(&mut self) {}

    /// "var" 変数名の並び ":" 型 ";" { 変数名の並び ":" 型 ";" }
    fn variable_declaration_part(&mut self) {}

    /// 変数名 { "," 変数名 }
    fn variable_names(&mut self) {}

    /// "名前"
    fn valriable_name(&mut self) {
        self.match_token(Kind::Name);
    }

    /// 標準型 | 配列型
    /// 予約語に引っかかるのを防ぐため，アンダーバーをつけている
    fn type_(&mut self) {}

    /// "integer" | "boolean" | "char"
    fn standard_type(&mut self) {
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Integer => self.match_token(Kind::Integer),
                Kind::Boolean => self.match_token(Kind::Boolean),
                Kind::Char => self.match_token(Kind::Char),
                _ => todo!(),
            },
            _ => todo!(),
        }
    }
}
