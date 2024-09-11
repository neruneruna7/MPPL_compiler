use crate::scan::scan3::{self, Kind, Lexer, Token};

mod error;
mod ast;
mod first_set;

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
pub enum SyntaxKind {
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

    fn match_consume_token(&mut self, kind: scan3::Kind) -> error::SyntaxResult {
        if self.match_token(kind) {
            self.cur_token = kind;
            self.lookahead = Some(self.lexer.read_next_token());
            println!("consume token: {:?}, lookahead: {:?}", kind, self.lookahead);
            Ok(())
        } else {
            println!(
                "consume token error: {:?}, lookahead: {:?}",
                kind, self.lookahead
            );
            Err(error::SyntaxError::new(self, &[kind], &[]))
        }
    }

    fn match_syntax_first_token(&self, syntax: SyntaxKind) -> bool {
        let lk = if let Some(ref l) = self.lookahead {
            l.kind
        } else {
            return false;
        };
        let binding = first_set::FIRST_SETS;
        let tokens = binding.iter().find(|x| x.symbol == syntax).unwrap();
        tokens.first_set.contains(&lk)
    }

    fn match_consume_syntax(&mut self, syntax: SyntaxKind) -> error::SyntaxResult {
        if self.match_syntax_first_token(syntax) {
            match syntax {
                SyntaxKind::Program => unreachable!(),
                SyntaxKind::Block => self.block()?,
                SyntaxKind::VariableDeclaration => self.variable_declaration_part()?,
                SyntaxKind::VariableNames => self.variable_names()?,
                SyntaxKind::VariableName => self.valriable_name()?,
                SyntaxKind::Type => self.type_()?,
                SyntaxKind::StandardType => self.standard_type()?,
                SyntaxKind::ArrayType => self.array_type()?,
                SyntaxKind::SubprogramDeclaration => self.subprogram_declaration()?,
                SyntaxKind::ProcedureName => self.procedure_name()?,
                SyntaxKind::FormalParameters => self.formal_parameters()?,
                SyntaxKind::CompoundStatement => self.compound_statement()?,
                SyntaxKind::Statement => self.statement()?,
                SyntaxKind::ConditionStatement => self.condnition_statement()?,
                SyntaxKind::IterationStatement => self.iteration_statement()?,
                SyntaxKind::ExitStatement => self.exit_statement()?,
                SyntaxKind::CallStatement => self.call_statement()?,
                SyntaxKind::Expressions => self.expressions()?,
                SyntaxKind::ReturnStatement => self.return_statement()?,
                SyntaxKind::AssignmentStatement => self.assignment_statement()?,
                SyntaxKind::LeftPart => self.left_part()?,
                SyntaxKind::Variable => self.variable()?,
                SyntaxKind::Expression => self.expression()?,
                SyntaxKind::SimpleExpression => self.simple_expression()?,
                SyntaxKind::Term => self.term()?,
                SyntaxKind::Factor => self.factor()?,
                SyntaxKind::Constant => self.constant()?,
                SyntaxKind::MultiplicativeOperator => self.multiplicative_operator()?,
                SyntaxKind::AdditiveOperator => self.additive_operator()?,
                SyntaxKind::RelationalOperator => self.relational_operator()?,
                SyntaxKind::InputStatement => self.input_statement()?,
                SyntaxKind::OutputStatement => self.output_statement()?,
                SyntaxKind::OutputFormat => self.output_format()?,
            }
            Ok(())
        } else {
            Err(error::SyntaxError::new(self, &[], &[syntax]))
        }
    }

    /// パースの開始
    /// "program" "名前" ";" ブロック "."
    pub fn parse_program(&mut self) -> error::SyntaxResult {
        // マクロ構文のprogramに該当
        self.match_consume_token(Kind::Program)?;
        self.match_consume_token(Kind::Name)?;
        self.match_consume_token(Kind::Semicolon)?;
        self.match_consume_syntax(SyntaxKind::Block)?;
        self.match_consume_token(Kind::Dot)?;
        Ok(())
    }

    /// { 変数宣言部 | 副プログラム宣言 } 複合文
    fn block(&mut self) -> error::SyntaxResult {
        while let Some(ref l) = self.lookahead {
            match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::VariableDeclaration) => {
                    self.match_consume_syntax(SyntaxKind::VariableDeclaration)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::SubprogramDeclaration) => {
                    self.match_consume_syntax(SyntaxKind::SubprogramDeclaration)?
                }
                _ => break,
            }
        }
        self.match_consume_syntax(SyntaxKind::CompoundStatement)?;
        Ok(())
    }

    /// "var" 変数名の並び ":" 型 ";" { 変数名の並び ":" 型 ";" }
    fn variable_declaration_part(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Var)?;
        self.match_consume_syntax(SyntaxKind::VariableNames)?;
        self.match_consume_token(Kind::Colon)?;
        self.match_consume_syntax(SyntaxKind::Type)?;
        self.match_consume_token(Kind::Semicolon)?;
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Name {
                self.match_consume_syntax(SyntaxKind::VariableNames)?;
                self.match_consume_token(Kind::Colon)?;
                self.match_consume_syntax(SyntaxKind::Type)?;
                self.match_consume_token(Kind::Semicolon)?;
            } else {
                break;
            }
        }

        Ok(())
    }

    /// 変数名 { "," 変数名 }
    fn variable_names(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::VariableName)?;

        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Comma {
                self.match_consume_token(Kind::Comma)?;
                self.match_consume_syntax(SyntaxKind::VariableName)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// "名前"
    fn valriable_name(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Name)?;
        Ok(())
    }

    /// 標準型 | 配列型
    /// 予約語に引っかかるのを防ぐため，アンダーバーをつけている
    fn type_(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(
            self,
            &[],
            &[SyntaxKind::StandardType, SyntaxKind::ArrayType],
        );
        match self.lookahead {
            Some(ref l) => match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::ArrayType) => self.array_type()?,
                _ if self.match_syntax_first_token(SyntaxKind::StandardType) => {
                    self.match_consume_syntax(SyntaxKind::StandardType)?
                }
                _ => return Err(err),
            },
            None => return Err(err),
        }
        Ok(())
    }

    /// "integer" | "boolean" | "char"
    fn standard_type(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[], &[SyntaxKind::StandardType]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Integer => self.match_consume_token(Kind::Integer)?,
                Kind::Boolean => self.match_consume_token(Kind::Boolean)?,
                Kind::Char => self.match_consume_token(Kind::Char)?,
                _ => return Err(err),
            },
            None => return Err(err),
        }
        Ok(())
    }

    /// "array" "[" "符号なし整数" "]" "of" 標準型
    fn array_type(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Array)?;
        self.match_consume_token(Kind::LBracket)?;
        self.match_consume_token(Kind::UnsignedInteger)?;
        self.match_consume_token(Kind::RBracket)?;
        self.match_consume_token(Kind::Of)?;
        self.match_consume_syntax(SyntaxKind::StandardType)?;
        Ok(())
    }

    /// "procedure" 手続き名 [ 仮引数部 ] ";" [ 変数宣言部 ] 複合文 ";"
    fn subprogram_declaration(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Procedure)?;
        self.match_consume_syntax(SyntaxKind::ProcedureName)?;
        if let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::FormalParameters) {
                self.match_consume_syntax(SyntaxKind::FormalParameters)?;
            }
        }
        self.match_consume_token(Kind::Semicolon)?;
        if let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::VariableDeclaration) {
                self.match_consume_syntax(SyntaxKind::VariableDeclaration)?;
            }
        }
        self.match_consume_syntax(SyntaxKind::CompoundStatement)?;
        self.match_consume_token(Kind::Semicolon)?;
        Ok(())
    }

    /// "名前"
    fn procedure_name(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Name)?;
        Ok(())
    }

    /// "(" 変数名の並び ":" 型 { ";" 変数名の並び ":" 型 } ")"
    fn formal_parameters(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::LParen)?;
        self.match_consume_syntax(SyntaxKind::VariableNames)?;
        self.match_consume_token(Kind::Colon)?;
        self.match_consume_syntax(SyntaxKind::Type)?;
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Semicolon {
                self.match_consume_token(Kind::Semicolon)?;
                self.match_consume_syntax(SyntaxKind::VariableNames)?;
                self.match_consume_token(Kind::Colon)?;
                self.match_consume_syntax(SyntaxKind::Type)?;
            } else {
                break;
            }
        }
        self.match_consume_token(Kind::RParen)?;
        Ok(())
    }

    /// "begin" 文 { ";" 文 } "end"
    fn compound_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Begin)?;
        self.match_consume_syntax(SyntaxKind::Statement)?;
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Semicolon {
                self.match_consume_token(Kind::Semicolon)?;
                self.match_consume_syntax(SyntaxKind::Statement)?;
            } else {
                break;
            }
        }
        self.match_consume_token(Kind::End)?;
        Ok(())
    }

    /// 代入文 | 分岐文 | 繰り返し文 | 脱出文 | 手続き呼び出し文 | 複合文 | 戻り文 | 入力文
    /// | 出力文 | 複合文 | 空文
    fn statement(&mut self) -> error::SyntaxResult {
        if let Some(ref l) = self.lookahead {
            match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::AssignmentStatement) => {
                    self.match_consume_syntax(SyntaxKind::AssignmentStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::ConditionStatement) => {
                    self.match_consume_syntax(SyntaxKind::ConditionStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::IterationStatement) => {
                    self.match_consume_syntax(SyntaxKind::IterationStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::ExitStatement) => {
                    self.match_consume_syntax(SyntaxKind::ExitStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::CallStatement) => {
                    self.match_consume_syntax(SyntaxKind::CallStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::CompoundStatement) => {
                    self.match_consume_syntax(SyntaxKind::CompoundStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::ReturnStatement) => {
                    self.match_consume_syntax(SyntaxKind::ReturnStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::InputStatement) => {
                    self.match_consume_syntax(SyntaxKind::InputStatement)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::OutputStatement) => {
                    self.match_consume_syntax(SyntaxKind::OutputStatement)?
                }
                // 空文なので何もしなくていい はず
                _ => {}
            }
        }
        Ok(())
    }

    /// "if" 式 "then" 文 [ "else" 文 ]
    fn condnition_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::If)?;
        self.match_consume_syntax(SyntaxKind::Expression)?;
        self.match_consume_token(Kind::Then)?;
        self.match_consume_syntax(SyntaxKind::Statement)?;

        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Else {
                self.match_consume_token(Kind::Else)?;
                self.match_consume_syntax(SyntaxKind::Statement)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// "while" 式 "do" 文
    fn iteration_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::While)?;
        self.match_consume_syntax(SyntaxKind::Expression)?;
        self.match_consume_token(Kind::DO)?;
        self.match_consume_syntax(SyntaxKind::Statement)?;
        Ok(())
    }

    /// "break"
    fn exit_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Break)?;
        Ok(())
    }

    /// "call" 手続き名 [ "(" 式の並び ")" ]
    fn call_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Call)?;
        self.match_consume_syntax(SyntaxKind::ProcedureName)?;
        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.match_consume_token(Kind::LParen)?;
                self.match_consume_syntax(SyntaxKind::Expressions)?;
                self.match_consume_token(Kind::RParen)?;
            }
        }
        Ok(())
    }

    /// 式 { "," 式 }
    fn expressions(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::Expression)?;
        while let Some(ref l) = self.lookahead {
            if l.kind == Kind::Comma {
                self.match_consume_token(Kind::Comma)?;
                self.match_consume_syntax(SyntaxKind::Expression)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// return
    fn return_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_token(Kind::Return)?;
        Ok(())
    }

    /// 左辺部 ":=" 式
    fn assignment_statement(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::LeftPart)?;
        self.match_consume_token(Kind::Assign)?;
        self.match_consume_syntax(SyntaxKind::Expression)?;
        Ok(())
    }

    /// 変数
    fn left_part(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::Variable)?;
        Ok(())
    }

    /// 変数名 [ "[" 式 "]" ]
    fn variable(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::VariableName)?;
        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LBracket {
                self.match_consume_token(Kind::LBracket)?;
                self.match_consume_syntax(SyntaxKind::Expression)?;
                self.match_consume_token(Kind::RBracket)?;
            }
        }
        Ok(())
    }

    /// 単純式 { 関係演算子 単純式 }
    fn expression(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::SimpleExpression)?;
        if let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::RelationalOperator) {
                self.match_consume_syntax(SyntaxKind::RelationalOperator)?;
                self.match_consume_syntax(SyntaxKind::SimpleExpression)?;
            }
        }
        Ok(())
    }

    /// [ "+" | "-" ] 項 { 加法演算子 項 }
    fn simple_expression(&mut self) -> error::SyntaxResult {
        if let Some(ref l) = self.lookahead {
            match l.kind {
                Kind::Plus => self.match_consume_token(Kind::Plus)?,
                Kind::Minus => self.match_consume_token(Kind::Minus)?,
                _ => {}
            }
        }
        self.match_consume_syntax(SyntaxKind::Term)?;
        while let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::AdditiveOperator) {
                self.match_consume_syntax(SyntaxKind::AdditiveOperator)?;
                self.match_consume_syntax(SyntaxKind::Term)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// 因子 { 乗法演算子 因子 }
    fn term(&mut self) -> error::SyntaxResult {
        self.match_consume_syntax(SyntaxKind::Factor)?;
        while let Some(ref l) = self.lookahead {
            if self.match_syntax_first_token(SyntaxKind::MultiplicativeOperator) {
                self.match_consume_syntax(SyntaxKind::MultiplicativeOperator)?;
                self.match_consume_syntax(SyntaxKind::Factor)?;
            } else {
                break;
            }
        }
        Ok(())
    }

    /// 変数 | 定数 | "(" 式 ")" | "not" 因子 | 標準型 "(" 式 ")"
    fn factor(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(
            self,
            &[Kind::LParen, Kind::Not],
            &[
                SyntaxKind::Variable,
                SyntaxKind::Constant,
                SyntaxKind::StandardType,
            ],
        );

        match self.lookahead {
            Some(ref l) => match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::Variable) => {
                    self.match_consume_syntax(SyntaxKind::Variable)?
                }
                _ if self.match_syntax_first_token(SyntaxKind::Constant) => {
                    self.match_consume_syntax(SyntaxKind::Constant)?
                }
                Kind::LParen => {
                    self.match_consume_token(Kind::LParen)?;
                    self.match_consume_syntax(SyntaxKind::Expression)?;
                    self.match_consume_token(Kind::RParen)?;
                }
                Kind::Not => {
                    self.match_consume_token(Kind::Not)?;
                    self.match_consume_syntax(SyntaxKind::Factor)?;
                }
                _ if self.match_syntax_first_token(SyntaxKind::StandardType) => {
                    self.match_consume_syntax(SyntaxKind::StandardType)?;
                    self.match_consume_token(Kind::LParen)?;
                    self.match_consume_syntax(SyntaxKind::Expression)?;
                    self.match_consume_token(Kind::RParen)?;
                }
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }

    /// "符号なし整数" | "true" | "false" | "文字列"
    fn constant(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(
            self,
            &[Kind::UnsignedInteger, Kind::True, Kind::False, Kind::String],
            &[],
        );
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::UnsignedInteger => self.match_consume_token(Kind::UnsignedInteger)?,
                Kind::True => self.match_consume_token(Kind::True)?,
                Kind::False => self.match_consume_token(Kind::False)?,
                Kind::String => self.match_consume_token(Kind::String)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }

    /// "*" | "div" | "and"
    fn multiplicative_operator(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[Kind::Star, Kind::Div, Kind::And], &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Star => self.match_consume_token(Kind::Star)?,
                Kind::Div => self.match_consume_token(Kind::Div)?,
                Kind::And => self.match_consume_token(Kind::And)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }

    /// "+" | "-" | "or"
    fn additive_operator(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[Kind::Plus, Kind::Minus, Kind::Or], &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Plus => self.match_consume_token(Kind::Plus)?,
                Kind::Minus => self.match_consume_token(Kind::Minus)?,
                Kind::Or => self.match_consume_token(Kind::Or)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }

    /// "=" | "<>" | "<" | "<=" | ">" | ">="
    fn relational_operator(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(
            self,
            &[
                Kind::Equal,
                Kind::NotEq,
                Kind::Less,
                Kind::LessEq,
                Kind::Great,
                Kind::GreatEq,
            ],
            &[],
        );
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Equal => self.match_consume_token(Kind::Equal)?,
                Kind::NotEq => self.match_consume_token(Kind::NotEq)?,
                Kind::Less => self.match_consume_token(Kind::Less)?,
                Kind::LessEq => self.match_consume_token(Kind::LessEq)?,
                Kind::Great => self.match_consume_token(Kind::Great)?,
                Kind::GreatEq => self.match_consume_token(Kind::GreatEq)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }

    /// ( "read" | "readln" ) [ "(" 変数 { "," 変数 } ")" ]
    fn input_statement(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[Kind::Read, Kind::Readln], &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Read => self.match_consume_token(Kind::Read)?,
                Kind::Readln => self.match_consume_token(Kind::Readln)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }

        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.match_consume_token(Kind::LParen)?;
                self.match_consume_syntax(SyntaxKind::Variable)?;
                while let Some(ref l) = self.lookahead {
                    if l.kind == Kind::Comma {
                        self.match_consume_token(Kind::Comma)?;
                        self.match_consume_syntax(SyntaxKind::Variable)?;
                    } else {
                        break;
                    }
                }
                self.match_consume_token(Kind::RParen)?;
            }
        }
        Ok(())
    }

    /// ( "write" | "writeln" ) [ "(" 出力指定 { "," 出力指定 } ")" ]
    fn output_statement(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[Kind::Write, Kind::Writeln], &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Write => self.match_consume_token(Kind::Write)?,
                Kind::Writeln => self.match_consume_token(Kind::Writeln)?,
                _ => Err(err)?,
            },
            None => Err(err)?,
        }

        if let Some(ref l) = self.lookahead {
            if l.kind == Kind::LParen {
                self.match_consume_token(Kind::LParen)?;
                self.match_consume_syntax(SyntaxKind::OutputFormat)?;
                while let Some(ref l) = self.lookahead {
                    if l.kind == Kind::Comma {
                        self.match_consume_token(Kind::Comma)?;
                        self.match_consume_syntax(SyntaxKind::OutputFormat)?;
                    } else {
                        break;
                    }
                }
                self.match_consume_token(Kind::RParen)?;
            }
        }
        Ok(())
    }

    /// 式 [ ":" "符号なし整数" ] | "文字列"
    fn output_format(&mut self) -> error::SyntaxResult {
        let err = error::SyntaxError::new(self, &[Kind::String], &[SyntaxKind::Expression]);

        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::String => self.match_consume_token(Kind::String)?,
                _ if self.match_syntax_first_token(SyntaxKind::Expression) => {
                    self.match_consume_syntax(SyntaxKind::Expression)?;

                    if let Some(ref l) = self.lookahead {
                        if l.kind == Kind::Colon {
                            self.match_consume_token(Kind::Colon)?;
                            self.match_consume_token(Kind::UnsignedInteger)?;
                        }
                    }
                }
                _ => Err(err)?,
            },
            None => Err(err)?,
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Parser;
    use crate::scan::scan3::Lexer;

    // ./parse/samples/1.mpl
    // ./parse/answes/1.mpl
    // を読み込む
    const TEST_SOURCE_COUNT: usize = 20;
    #[test]
    fn tests_parser() {
        for i in 1..=TEST_SOURCE_COUNT {
            let source =
                std::fs::read_to_string(format!("test_source/perse/samples/{}.mpl", i)).unwrap();
            let answer =
                std::fs::read_to_string(format!("test_source/perse/answers/{}.mpl", i)).unwrap();
            let lexer = Lexer::new(&source);
            let mut parser = Parser::new(lexer);
            match parser.parse_program() {
                Ok(_) => println!("{} parsing OK \n{}", i, source),
                Err(e) => {
                    eprintln!("Err {}:  {}", i, e);
                    // assert_eq!(e.lexeicalized_source.trim(), answer.trim())
                    // ソースコードと正解例をトークン化し，トークン化した結果を比較する
                    // トークンの位置情報はいらないので，取り除いたものを比較する
                    let mut lex = Lexer::new(&e.lexeicalized_source);
                    let lexeicalized_err_source = lex
                        .analyze()
                        .iter()
                        .map(|t| (t.kind, t.value.clone()))
                        .collect::<Vec<_>>();
                    let mut lex = Lexer::new(&answer);
                    let lexeicalized_answer = lex
                        .analyze()
                        .iter()
                        .map(|t| (t.kind, t.value.clone()))
                        .collect::<Vec<_>>();
                    assert_eq!(lexeicalized_err_source, lexeicalized_answer);
                }
            }
        }
    }
}
