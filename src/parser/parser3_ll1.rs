use std::{cell::LazyCell, collections::HashSet};

use crate::scan3::{self, Kind, Lexer, Token};

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
enum SyntaxKind {
    Program,
    Block,
    VariableDeclaration,
    VariableNames,
    VariableName,
    Type,
    StandardType,
    ArrayType,
    SubprogramDeclaration,
    ProcedureName,
    FormalParameters,
    CompoundStatement,
    Statement,
    ConditionStatement,
    IterationStatement,
    ExitStatement,
    CallStatement,
    Expressions,
    ReturnStatement,
    AssignmentStatement,
    LeftPart,
    Variable,
    Expression,
    SimpleExpression,
    Term,
    Factor,
    Constant,
    MultiplicativeOperator,
    AdditiveOperator,
    RelationalOperator,
    InputStatement,
    OutputStatement,
    OutputFormat,
}

struct FirstSet {
    symbol: SyntaxKind,
    first_set: HashSet<Kind>,
}

impl FirstSet {
    fn new(symbol: SyntaxKind, first_set: HashSet<Kind>) -> Self {
        Self { symbol, first_set }
    }
}


const FIRST_SETS: LazyCell<Vec<FirstSet>> = {
    LazyCell::new(|| {
        vec![
            FirstSet::new(SyntaxKind::Program, vec![Kind::Program].into_iter().collect()),
            FirstSet::new(
                SyntaxKind::Block,
                vec![Kind::Var, Kind::Procedure, Kind::Begin].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::VariableDeclaration,
                vec![Kind::Var].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::VariableNames,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::VariableName,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(SyntaxKind::Type, vec![Kind::Integer, Kind::Boolean, Kind::Char, Kind::Array].into_iter().collect()),
            FirstSet::new(
                SyntaxKind::StandardType,
                vec![Kind::Integer, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::ArrayType,
                vec![Kind::Array].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::SubprogramDeclaration,
                vec![Kind::Procedure].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::ProcedureName,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::FormalParameters,
                vec![Kind::LParen].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::CompoundStatement,
                vec![Kind::Begin].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Statement,
                vec![
                    Kind::Name,
                    Kind::If,
                    Kind::While,
                    Kind::Break,
                    Kind::Call,
                    Kind::Return,
                    Kind::Read,
                    Kind::Readln,
                    Kind::Write,
                    Kind::Writeln,
                    Kind::Begin,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::ConditionStatement,
                vec![Kind::If].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::IterationStatement,
                vec![Kind::While].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::ExitStatement,
                vec![Kind::Break].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::CallStatement,
                vec![Kind::Call].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Expressions,
                vec![Kind::Name, Kind::Plus, Kind::Minus, Kind::LParen, Kind::Not, Kind::Integer, Kind::True, Kind::False, Kind::String, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::ReturnStatement,
                vec![Kind::Return].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::AssignmentStatement,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::LeftPart,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Variable,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Expression,
                vec![Kind::Name, Kind::Plus, Kind::Minus, Kind::LParen, Kind::Not, Kind::Integer, Kind::True, Kind::False, Kind::String, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::SimpleExpression,
                vec![Kind::Name, Kind::Plus, Kind::Minus, Kind::LParen, Kind::Not, Kind::Integer, Kind::True, Kind::False, Kind::String, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Term,
                vec![Kind::Name, Kind::Plus, Kind::Minus, Kind::LParen, Kind::Not, Kind::Integer, Kind::True, Kind::False, Kind::String, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Factor,
                vec![Kind::Name, Kind::LParen, Kind::Not, Kind::Integer, Kind::True, Kind::False, Kind::String, Kind::Boolean, Kind::Char].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Constant,
                vec![Kind::UnsignedInteger, Kind::True, Kind::False, Kind::String].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::MultiplicativeOperator,
                vec![Kind::Star, Kind::Div, Kind::And].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::AdditiveOperator,
                vec![Kind::Plus, Kind::Minus, Kind::Or].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::RelationalOperator,
                vec![Kind::Equal, Kind::NotEq, Kind::Less, Kind::LessEq, Kind::Great, Kind::GreatEq].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::InputStatement,
                vec![Kind::Read, Kind::Readln].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::OutputStatement,
                vec![Kind::Write, Kind::Writeln].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::OutputFormat,
                vec![Kind::Plus, Kind::Minus, Kind::String].into_iter().collect(),
            ),
        ]
    })
};

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    lookahead: Option<Token>,
    cur_token: Kind,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let init_token = lexer.read_next_token();
        Self {
            lexer,
            lookahead: Some(init_token),
            cur_token: Kind::Program,
        }
    }

    fn match_token(&self, kind: scan3::Kind) -> bool {
        match self.lookahead {
            Some(ref l) => l.kind == kind,
            None => false,
        }
    }

    fn match_syntax_first_token(&self, syntax: SyntaxKind) -> bool {
        let lk = if let Some(ref l) = self.lookahead {
            l.kind
        } else {
            return false;
        };
        let binding = FIRST_SETS;
        let tokens = binding
            .iter()
            .find(|x| x.symbol == syntax)
            .unwrap();
        tokens.first_set.contains(&lk)
    }

    fn match_consume_token(&mut self, kind: scan3::Kind) {
        if let Some(ref l) = self.lookahead {
            if l.kind == kind {
                self.cur_token = kind;
                self.lookahead = Some(self.lexer.read_next_token());
                println!("consume token: {:?}, lookahead: {:?}", kind, self.lookahead);

                return;
            }
        }

        self.syntax_error([kind].as_ref(), [].as_ref());
    }

    fn syntax_error(&self, expected_token: &[scan3::Kind], expected_syntax: &[SyntaxKind]) {
        let sliced_source = if let Some(ref l) = self.lookahead {
            &self.lexer.source[..l.start]
        } else {
            self.lexer.source
        };

        panic!(
            "syntax error source code:\n\n {} \n\n expect token= {:?}, expect syntax= {:?} but found {:?}",
            sliced_source, expected_token, expected_syntax, &self.lookahead
        );
    }

    /// パースの開始
    /// "program" "名前" ";" ブロック "."
    pub fn parse_program(&mut self) {
        // マクロ構文のprogramに該当
        self.match_consume_token(Kind::Program);
        self.match_consume_token(Kind::Name);
        self.match_consume_token(Kind::Semicolon);
        self.block();
        self.match_consume_token(Kind::Dot);
    }

    /// { 変数宣言部 | 副プログラム宣言 } 複合文
    fn block(&mut self) {
        while let Some(ref l) = self.lookahead {
            match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::VariableDeclaration) => {
                    self.variable_declaration_part()
                }
                _ if self.match_syntax_first_token(SyntaxKind::SubprogramDeclaration) => {
                    self.subprogram_declaration()
                }
                _ => break,
            }
        }
        self.compound_statement();
    }

    /// "var" 変数名の並び ":" 型 ";" { 変数名の並び ":" 型 ";" }
    fn variable_declaration_part(&mut self) {
        self.match_consume_token(Kind::Var);
        self.variable_names();
        self.match_consume_token(Kind::Colon);
        self.type_();
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
        let err = || self.syntax_error(&[], &[SyntaxKind::StandardType, SyntaxKind::ArrayType]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::ArrayType) => self.array_type(),
                _ if self.match_syntax_first_token(SyntaxKind::StandardType) => {
                    self.standard_type()
                }
                _ => err(),
            },
            None => err(),
        }
    }

    /// "integer" | "boolean" | "char"
    fn standard_type(&mut self) {
        let err = || self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Integer => self.match_consume_token(Kind::Integer),
                Kind::Boolean => self.match_consume_token(Kind::Boolean),
                Kind::Char => self.match_consume_token(Kind::Char),
                _ => err(),
            },
            None => err(),
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
    fn subprogram_declaration(&mut self) {
        self.match_consume_token(Kind::Procedure);
        self.procedure_name();
        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.formal_parameters();
            }
        }
        self.match_consume_token(Kind::Semicolon);
        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::Var {
                self.variable_declaration_part();
            }
        }
        self.compound_statement();
        self.match_consume_token(Kind::Semicolon);
    }

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
    fn statement(&mut self) {
        if let Some(ref l) = self.lookahead {
            match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::AssignmentStatement) => {
                    self.assignment_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::ConditionStatement) => {
                    self.condnition_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::IterationStatement) => {
                    self.iteration_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::ExitStatement) => {
                    self.exit_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::CallStatement) => {
                    self.call_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::CompoundStatement) => {
                    self.compound_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::ReturnStatement) => {
                    self.return_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::InputStatement) => {
                    self.input_statement()
                }
                _ if self.match_syntax_first_token(SyntaxKind::OutputStatement) => {
                    self.output_statement()
                }
                _ => {}
            }
        }
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
        self.expression();
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
        self.expressions();
        self.match_consume_token(Kind::RParen);
    }

    /// 式 { "," 式 }
    fn expressions(&mut self) {
        self.expression();
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Comma {
                self.match_consume_token(Kind::Comma);
                self.expression();
            } else {
                break;
            }
        }
    }

    /// return
    fn return_statement(&mut self) {
        self.match_consume_token(Kind::Return);
    }

    /// 左辺部 ":=" 式
    fn assignment_statement(&mut self) {
        self.left_part();
        self.match_consume_token(Kind::Assign);
        self.expression();
    }

    /// 変数
    fn left_part(&mut self) {
        self.variable();
    }

    /// 変数名 [ "[" 式 "]" ]
    fn variable(&mut self) {
        self.valriable_name();
        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LBracket {
                self.match_consume_token(Kind::LBracket);
                self.expression();
                self.match_consume_token(Kind::RBracket);
            }
        }
    }

    /// 単純式 { 関係演算子 単純式 }
    fn expression(&mut self) {
        self.simple_expression();
        if let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::RelationalOperator) {
                self.relational_operator();
                self.simple_expression();
            }
        }
    }

    /// [ "+" | "-" ] 項 { 加法演算子 項 }
    fn simple_expression(&mut self) {
        if let Some(ref l) = self.lookahead {
            match l.kind {
                Kind::Plus => self.match_consume_token(Kind::Plus),
                Kind::Minus => self.match_consume_token(Kind::Minus),
                _ => {}
            }
        }

        self.term();
        while let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::AdditiveOperator) {
                self.additive_operator();
                self.term();
            } else {
                break;
            }
        }
    }

    /// 因子 { 乗法演算子 因子 }
    fn term(&mut self) {
        self.factor();
        while let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::MultiplicativeOperator) {
                self.multiplicative_operator();
                self.factor();
            } else {
                break;
            }
        }
    }

    /// 変数 | 定数 | "(" 式 ")" | "not" 因子 | 標準型 "(" 式 ")"
    fn factor(&mut self) {
        let err = || {
            self.syntax_error(
                [Kind::LParen, Kind::Not].as_ref(),
                &[
                    SyntaxKind::Variable,
                    SyntaxKind::Constant,
                    SyntaxKind::StandardType,
                ],
            )
        };

        match self.lookahead {
            Some(ref l) => match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::Variable) => self.variable(),
                _ if self.match_syntax_first_token(SyntaxKind::Constant) => self.constant(),
                Kind::LParen => {
                    self.match_consume_token(Kind::LParen);
                    self.expression();
                    self.match_consume_token(Kind::RParen);
                }
                Kind::Not => {
                    self.match_consume_token(Kind::Not);
                    self.factor();
                }
                _ if self.match_syntax_first_token(SyntaxKind::StandardType) => {
                    self.standard_type();
                    self.match_consume_token(Kind::LParen);
                    self.expression();
                    self.match_consume_token(Kind::RParen);
                }
                _ => err(),
            },
            None => err(),
        }
    }

    /// "符号なし整数" | "true" | "false" | "文字列"
    fn constant(&mut self) {
        let err = || {
            self.syntax_error(
                [Kind::UnsignedInteger, Kind::True, Kind::False, Kind::String].as_ref(),
                &[],
            )
        };
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::UnsignedInteger => self.match_consume_token(Kind::UnsignedInteger),
                Kind::True => self.match_consume_token(Kind::True),
                Kind::False => self.match_consume_token(Kind::False),
                Kind::String => self.match_consume_token(Kind::String),
                _ => err(),
            },
            None => err(),
        }
    }

    /// "*" | "div" | "and"
    fn multiplicative_operator(&mut self) {
        let err = || self.syntax_error([Kind::Star, Kind::Div, Kind::And].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Star => self.match_consume_token(Kind::Star),
                Kind::Div => self.match_consume_token(Kind::Div),
                Kind::And => self.match_consume_token(Kind::And),
                _ => err(),
            },
            None => err(),
        }
    }

    /// "+" | "-" | "or"
    fn additive_operator(&mut self) {
        let err = || self.syntax_error([Kind::Plus, Kind::Minus, Kind::Or].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Plus => self.match_consume_token(Kind::Plus),
                Kind::Minus => self.match_consume_token(Kind::Minus),
                Kind::Or => self.match_consume_token(Kind::Or),
                _ => err(),
            },
            None => err(),
        }
    }

    /// "=" | "<>" | "<" | "<=" | ">" | ">="
    fn relational_operator(&mut self) {
        let err = || {
            self.syntax_error(
                [
                    Kind::Equal,
                    Kind::NotEq,
                    Kind::Less,
                    Kind::LessEq,
                    Kind::Great,
                    Kind::GreatEq,
                ]
                .as_ref(),
                &[],
            )
        };
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Equal => self.match_consume_token(Kind::Equal),
                Kind::NotEq => self.match_consume_token(Kind::NotEq),
                Kind::Less => self.match_consume_token(Kind::Less),
                Kind::LessEq => self.match_consume_token(Kind::LessEq),
                Kind::Great => self.match_consume_token(Kind::Great),
                Kind::GreatEq => self.match_consume_token(Kind::GreatEq),
                _ => err(),
            },
            None => err(),
        }
    }

    /// ( "read" | "readln" ) [ "(" 変数 { "," 変数 } ")" ]
    fn input_statement(&mut self) {
        let err = || self.syntax_error([Kind::Read, Kind::Readln].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Read => self.match_consume_token(Kind::Read),
                Kind::Readln => self.match_consume_token(Kind::Readln),
                _ => err(),
            },
            None => err(),
        }

        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.match_consume_token(Kind::LParen);
                self.variable();
                while let Some(ref l) = self.lookahead {
                    if l.kind == Kind::Comma {
                        self.match_consume_token(Kind::Comma);
                        self.variable();
                    } else {
                        break;
                    }
                }
                self.match_consume_token(Kind::RParen);
            }
        }
    }

    /// ( "write" | "writeln" ) [ "(" 出力指定 { "," 出力指定 } ")" ]
    fn output_statement(&mut self) {
        let err = || self.syntax_error([Kind::Write, Kind::Writeln].as_ref(), &[]);

        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Write => self.match_consume_token(Kind::Write),
                Kind::Writeln => self.match_consume_token(Kind::Writeln),
                _ => err(),
            },
            None => err(),
        }

        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.match_consume_token(Kind::LParen);
                self.output_format();
                while let Some(ref l) = self.lookahead {
                    if l.kind == Kind::Comma {
                        self.match_consume_token(Kind::Comma);
                        self.output_format();
                    } else {
                        break;
                    }
                }
                self.match_consume_token(Kind::RParen);
            }
        }
    }

    /// 式 [ ":" "符号なし整数" ] | "文字列"
    fn output_format(&mut self) {
        let err = || self.syntax_error([Kind::Plus, Kind::Minus, Kind::String].as_ref(), &[]);

        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::String => self.match_consume_token(Kind::String),
                _ if self.match_syntax_first_token(SyntaxKind::Expression) => {
                    self.expression();

                    if let Some(ref l) = self.lookahead {
                        if l.kind == Kind::Colon {
                            self.match_consume_token(Kind::Colon);
                            self.match_consume_token(Kind::UnsignedInteger);
                        }
                    }
                }
                _ => err(),
            },
            None => err(),
        }
    }
}
