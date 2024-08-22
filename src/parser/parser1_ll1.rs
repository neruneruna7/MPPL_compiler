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

    fn match_token(&self, kind: scan3::Kind) -> bool {
        match self.lookahead {
            Some(ref l) => l.kind == kind,
            None => false,
        }
    }

    fn match_consume_token(&mut self, kind: scan3::Kind) {
        match self.lookahead {
            Some(ref l) => {
                if l.kind == kind {
                    self.lookahead = Some(self.lexer.read_next_token());
                    return;
                }
            }
            None => {}
        }

        self.syntax_error([kind].as_ref());
    }

    fn syntax_error(&self, expected: &[scan3::Kind]) {
        let sliced_source = if let Some(ref l) = self.lookahead {
            &self.lexer.source[..l.start]
        } else {
            &self.lexer.source
        };

        panic!(
            "syntax error source code:\n\n {} \n\n expect {:?} but found {:?}",
            sliced_source,
            expected,
            &self.lookahead
        );
    }

    /// パースの開始
    /// "program" "名前" ";" ブロック "."
    pub fn parse_program(&mut self) {
        // マクロ構文のprogramに該当
        self.match_consume_token(Kind::Program);
        self.match_consume_token(Kind::Name);
        self.match_consume_token(Kind::Semicolon);
        // self.block();
        self.match_consume_token(Kind::Dot);
    }

    /// { 変数宣言部 | 副プログラム宣言 } 複合文
    fn block(&mut self) {}

    /// "var" 変数名の並び ":" 型 ";" { 変数名の並び ":" 型 ";" }
    fn variable_declaration_part(&mut self) {
        self.match_consume_token(Kind::Var);
        self.variable_names();
        self.match_consume_token(Kind::Semicolon);
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Name {
                self.variable_names();
                self.match_consume_token(Kind::Colon);
                self.type_();
                self.match_consume_token(Kind::Semicolon);
            } else {
                break;
            }
        }
    }

    /// 変数名 { "," 変数名 }
    fn variable_names(&mut self) {
        self.valriable_name();
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Comma {
                self.match_consume_token(Kind::Comma);
                self.valriable_name();
            } else {
                break;
            }
        }
    }

    /// "名前"
    fn valriable_name(&mut self) {
        self.match_consume_token(Kind::Name);
    }

    /// 標準型 | 配列型
    /// 予約語に引っかかるのを防ぐため，アンダーバーをつけている
    fn type_(&mut self) {
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Array => self.array_type(),
                Kind::Integer | Kind::Boolean | Kind::Char => self.standard_type(),
                _ => self.syntax_error([Kind::Array, Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
            },
            _ => self.syntax_error([Kind::Array, Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
        }
    }

    /// "integer" | "boolean" | "char"
    fn standard_type(&mut self) {
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Integer => self.match_consume_token(Kind::Integer),
                Kind::Boolean => self.match_consume_token(Kind::Boolean),
                Kind::Char => self.match_consume_token(Kind::Char),
                _ => self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
            },
            _ => self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
        }
    }

    /// "array" "[" "符号なし整数" "]" "of" 標準型
    fn array_type(&mut self) {
        self.match_consume_token(Kind::Array);
        self.match_consume_token(Kind::LBracket);
        self.match_consume_token(Kind::UnsignedInteger);
        self.match_consume_token(Kind::RBracket);
        self.match_consume_token(Kind::Of);
        self.standard_type();
    }

    /// "procedure" 手続き名 [ 仮引数部 ] ";" [ 変数宣言部 ] 複合文 ";"
    fn subprogram_declaration(&mut self) {}

    /// "名前"
    fn procedure_name(&mut self) {
        self.match_consume_token(Kind::Name);
    }

    /// "(" 変数名の並び ":" 型 { ";" 変数名の並び ":" 型 } ")"
    fn formal_parameters(&mut self) {
        self.match_consume_token(Kind::LParen);
        self.variable_names();
        self.match_consume_token(Kind::Colon);
        self.type_();
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Semicolon {
                self.match_consume_token(Kind::Semicolon);
                self.variable_names();
                self.match_consume_token(Kind::Colon);
                self.type_();
            } else {
                break;
            }
        }
        self.match_consume_token(Kind::RParen);
    }

    /// "begin" 文 { ";" 文 } "end"
    fn compound_statement(&mut self) {
        self.match_consume_token(Kind::Begin);
        self.statement();
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Semicolon {
                self.match_consume_token(Kind::Semicolon);
                self.statement();
            } else {
                break;
            }
        }
        self.match_consume_token(Kind::End);
    }
    
    /// 代入文 | 分岐文 | 繰り返し文 | 脱出文 | 手続き呼び出し文 | 複合文 | 戻り文 | 入力文
    /// | 出力文 | 複合文 | 空文
    fn statement(&self) {
        todo!()
    }

    /// "if" 式 "then" 文 [ "else" 文 ]
    fn condnition_statement(&mut self) {
        self.match_consume_token(Kind::If);
        // self.expression();
        self.match_consume_token(Kind::Then);
        self.statement();
        self.match_consume_token(Kind::Else);
        self.statement();
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Else {
                self.match_consume_token(Kind::Else);
                self.statement();
            } else {
                break;
            }
        }
    }

    /// "while" 式 "do" 文
    fn iteration_statement(&mut self) {
        self.match_consume_token(Kind::While);
        // self.expression();
        self.match_consume_token(Kind::DO);
        self.statement();
    }

    /// "break"
    fn exit_statement(&mut self) {
        self.match_consume_token(Kind::Break);
    }

    /// "call" 手続き名 [ "(" 式の並び ")" ]
    fn call_statement(&mut self) {
        self.match_consume_token(Kind::Call);
        self.procedure_name();
        self.match_consume_token(Kind::LParen);
        // self.expression();
        self.match_consume_token(Kind::RParen);
    }

    /// 式 { "," 式 }
    fn expressions(&mut self) {
        todo!()
    }

    /// return
    fn return_statement(&mut self) {
        self.match_consume_token(Kind::Return);
    }

    /// 左辺部 ":=" 式
    fn assignment_statement(&mut self) {
        todo!()
    }

    /// 変数
    fn left_part(&mut self) {
        todo!()
    }

    /// 変数名 [ "[" 式 "]" ]
    fn variable(&mut self) {
        todo!()
    }

    /// 単純式 { 関係演算子 単純式 }
    fn expression(&mut self) {
        todo!()
    }

    /// [ "+" | "-" ] 項 { 加法演算子 項 }
    fn simple_expression(&mut self) {
        todo!()
    }


    /// 因子 { 乗法演算子 因子 }
    fn term(&mut self) {
        todo!()
    }

    /// 変数 | 定数 | "(" 式 ")" | "not" 因子 | 標準型 "(" 式 ")"
    fn factor(&mut self) {
        todo!()
    }

    /// "符号なし整数" | "true" | "false" | "文字列"
    fn constant(&mut self) {
        // match self.lookahead {
        //     Some(ref l) => match l.kind {
        //         Kind::Integer => self.match_consume_token(Kind::Integer),
        //         Kind::True => self.match_consume_token(Kind::True),
        //         Kind::False => self.match_consume_token(Kind::False),
        //         Kind:: => self.match_consume_token(Kind::String),
        //         _ => self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
        //     },
        //     _ => self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref()),
        // }
    }







    
}
