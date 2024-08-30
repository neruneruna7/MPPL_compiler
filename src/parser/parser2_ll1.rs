use crate::scan3::{self, Kind, Lexer, Token};

// エラー時に，期待したトークンの集合を返したかったが，いま試している方法ではうまくいかなさそうだ
// FIRST集合を求めれば，それをもとにできそうだが


type Expected = Vec<scan3::Kind>;
// 独自のエラー型を定義
// どんなトークンを期待していたが，実際にはどんなトークンが来たかを表現する
#[derive(Debug)]
pub struct SyntaxError {
    lexeicalized_source: String,
    expected_token: Vec<scan3::Kind>,
    found: Option<Token>,
}

impl SyntaxError {
    pub fn new(
        perser: &Parser,
        expected_token: Vec<scan3::Kind>,
        found: Option<Token>,
    ) -> Self {
        let sliced_source = if let Some(ref l) = perser.lookahead {
            &perser.lexer.source[..l.start]
        } else {
            perser.lexer.source
        };
        Self {
            lexeicalized_source: sliced_source.to_string(),
            expected_token,
            found,
        }
    }

    pub fn append_expected_token(&mut self, token: &[Kind]) {
        self.expected_token.extend_from_slice(token);
    }


    pub fn compose(
        &mut self,
        other: SyntaxResult,
    ){
        match other {
            Ok(t) => {
                self.append_expected_token(&t);
            }
            Err(e) => {
                self.expected_token.extend_from_slice(&e.expected_token);
            }
        }
    }
}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let expected_token = self
            .expected_token
            .iter()
            .map(|k| format!("{:?}", k))
            .collect::<Vec<String>>()
            .join(", ");
        let found = if let Some(ref l) = self.found {
            format!("{:?}", l)
        } else {
            "None".to_string()
        };

        write!(
            f,
            "source code:\n\n {} \n\n expect token= {:?} but found {:?}",
            self.lexeicalized_source, expected_token,  found
        )
    }
}

impl std::error::Error for SyntaxError {}


type SyntaxResult = std::result::Result<Expected, SyntaxError>;

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

    fn match_syntax_first_token(&self, syntax: SyntaxKind) -> bool {
        let lk = if let Some(ref l) = self.lookahead {
            l.kind
        } else {
            return false;
        };
        match syntax {
            SyntaxKind::Program => lk == Kind::Program,
            SyntaxKind::Block => {
                self.match_syntax_first_token(SyntaxKind::VariableDeclaration)
                    || self.match_syntax_first_token(SyntaxKind::SubprogramDeclaration)
                    || self.match_syntax_first_token(SyntaxKind::CompoundStatement)
            }
            SyntaxKind::VariableDeclaration => lk == Kind::Var,
            SyntaxKind::VariableNames => self.match_syntax_first_token(SyntaxKind::VariableName),
            SyntaxKind::VariableName => lk == Kind::Name,
            SyntaxKind::Type => {
                self.match_syntax_first_token(SyntaxKind::StandardType)
                    || self.match_syntax_first_token(SyntaxKind::ArrayType)
            }
            SyntaxKind::StandardType => match lk {
                Kind::Integer | Kind::Boolean | Kind::Char => true,
                _ => false,
            },
            SyntaxKind::ArrayType => lk == Kind::Array,
            SyntaxKind::SubprogramDeclaration => lk == Kind::Procedure,
            SyntaxKind::ProcedureName => lk == Kind::Name,
            SyntaxKind::FormalParameters => lk == Kind::LParen,
            SyntaxKind::CompoundStatement => lk == Kind::Begin,
            SyntaxKind::Statement => {
                self.match_syntax_first_token(SyntaxKind::AssignmentStatement)
                    || self.match_syntax_first_token(SyntaxKind::ConditionStatement)
                    || self.match_syntax_first_token(SyntaxKind::IterationStatement)
                    || self.match_syntax_first_token(SyntaxKind::ExitStatement)
                    || self.match_syntax_first_token(SyntaxKind::CallStatement)
                    || self.match_syntax_first_token(SyntaxKind::ReturnStatement)
                    || self.match_syntax_first_token(SyntaxKind::InputStatement)
                    || self.match_syntax_first_token(SyntaxKind::OutputStatement)
                    || self.match_syntax_first_token(SyntaxKind::CompoundStatement)
            }
            SyntaxKind::ConditionStatement => lk == Kind::If,
            SyntaxKind::IterationStatement => lk == Kind::While,
            SyntaxKind::ExitStatement => lk == Kind::Break,
            SyntaxKind::CallStatement => lk == Kind::Call,
            SyntaxKind::Expressions => self.match_syntax_first_token(SyntaxKind::Expression),
            SyntaxKind::ReturnStatement => lk == Kind::Return,
            SyntaxKind::AssignmentStatement => self.match_syntax_first_token(SyntaxKind::LeftPart),
            SyntaxKind::LeftPart => self.match_syntax_first_token(SyntaxKind::Variable),
            SyntaxKind::Variable => self.match_syntax_first_token(SyntaxKind::VariableName),
            SyntaxKind::Expression => self.match_syntax_first_token(SyntaxKind::SimpleExpression),
            SyntaxKind::SimpleExpression => {
                lk == Kind::Plus
                    || lk == Kind::Minus
                    || self.match_syntax_first_token(SyntaxKind::Term)
            }
            SyntaxKind::Term => self.match_syntax_first_token(SyntaxKind::Factor),
            SyntaxKind::Factor => {
                self.match_syntax_first_token(SyntaxKind::Variable)
                    || self.match_syntax_first_token(SyntaxKind::Constant)
                    || self.match_token(Kind::LParen)
                    || self.match_token(Kind::Not)
                    || self.match_syntax_first_token(SyntaxKind::StandardType)
            }
            SyntaxKind::Constant => match lk {
                Kind::UnsignedInteger | Kind::True | Kind::False | Kind::String => true,
                _ => false,
            },
            SyntaxKind::MultiplicativeOperator => match lk {
                Kind::Star | Kind::Div | Kind::And => true,
                _ => false,
            },
            SyntaxKind::AdditiveOperator => match lk {
                Kind::Plus | Kind::Minus | Kind::Or => true,
                _ => false,
            },
            SyntaxKind::RelationalOperator => match lk {
                Kind::Equal
                | Kind::NotEq
                | Kind::Less
                | Kind::LessEq
                | Kind::Great
                | Kind::GreatEq => true,
                _ => false,
            },
            SyntaxKind::InputStatement => lk == Kind::Read || lk == Kind::Readln,
            SyntaxKind::OutputStatement => lk == Kind::Write || lk == Kind::Writeln,
            SyntaxKind::OutputFormat => self.match_syntax_first_token(SyntaxKind::Expression),
        }
    }

    fn match_consume_token(&mut self, kind: scan3::Kind) -> SyntaxResult {
        let et = [kind];
        let mut se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
        if let Some(ref l) = self.lookahead {
            if l.kind == kind {
                self.lookahead = Some(self.lexer.read_next_token());
                return Ok(et.to_vec());
            }
        }

        // self.syntax_error([kind].as_ref(), [].as_ref());
        Err(se)    }

    // fn match_consume_token_test(&mut self, kind: scan3::Kind) -> SyntaxResult {
    //     let et = [kind];
    //     let mut se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
    //     if let Some(ref l) = self.lookahead {
    //         if l.kind == kind {
    //             self.lookahead = Some(self.lexer.read_next_token());
    //             return Ok(et.to_vec());
    //         }
    //     }

    //     // self.syntax_error([kind].as_ref(), [].as_ref());
    //     Err(se)
    // }

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
    pub fn parse_program(&mut self) -> SyntaxResult {
        // マクロ構文のprogramに該当
        self.match_consume_token(Kind::Program)?;
        self.match_consume_token(Kind::Name);
        self.match_consume_token(Kind::Semicolon);
        self.block();
        self.match_consume_token(Kind::Dot);
        Ok([].to_vec())
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
    fn type_(&mut self) -> SyntaxResult {
        let et = [Kind::StandardType, Kind::ArrayType];
        let se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
        // let err = || self.syntax_error(&[], &[SyntaxKind::StandardType, SyntaxKind::ArrayType]);
        let err = || Err(se);
        
        match self.lookahead {
            Some(ref l) => match l.kind {
                _ if self.match_syntax_first_token(SyntaxKind::ArrayType) => self.array_type(),
                _ if self.match_syntax_first_token(SyntaxKind::StandardType) => {
                    self.standard_type()
                }
                _ => err()?,
            },
            None => err()?,
        }
    }

    /// "integer" | "boolean" | "char"
    fn standard_type(&mut self) -> SyntaxResult {
        let et = [Kind::Integer, Kind::Boolean, Kind::Char];
        let se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
        let err = || self.syntax_error([Kind::Integer, Kind::Boolean, Kind::Char].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Integer => self.match_consume_token(Kind::Integer),
                Kind::Boolean => self.match_consume_token(Kind::Boolean),
                Kind::Char => self.match_consume_token(Kind::Char),
                _ => Err(se)?,
            },
            None => Err(se)?,
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
        let mut se = SyntaxError::new(
            self,
            [Kind::LParen, Kind::Not].to_vec(),
            self.lookahead.clone(),
        );
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
                _ if self.match_syntax_first_token(SyntaxKind::Constant) => se.compose(self.constant()),
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
                _ => Err(se).unwrap(),
            },
            None => err(),
        }
    }

    /// "符号なし整数" | "true" | "false" | "文字列"
    fn constant(&mut self) -> SyntaxResult {
        let expected_token = [Kind::UnsignedInteger, Kind::True, Kind::False, Kind::String];
        let mut syntax_err = SyntaxError::new(
            self,
            expected_token.to_vec(),
            self.lookahead.clone(),
        );

        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::UnsignedInteger => self.match_consume_token(Kind::UnsignedInteger),
                Kind::True => self.match_consume_token(Kind::True),
                Kind::False => self.match_consume_token(Kind::False),
                Kind::String => self.match_consume_token(Kind::String),
                _ => Err(syntax_err)?,
            },
            None => Err(syntax_err)?,
        }

        Ok(expected_token.to_vec())
    }

    /// "*" | "div" | "and"
    fn multiplicative_operator(&mut self) -> SyntaxResult {

        let et = [Kind::Star, Kind::Div, Kind::And];
        let mut se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());

        let err = || self.syntax_error([Kind::Star, Kind::Div, Kind::And].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Star => self.match_consume_token(Kind::Star),
                Kind::Div => self.match_consume_token(Kind::Div),
                Kind::And => self.match_consume_token(Kind::And),
                _ => 
                    Err(se)?,
            },
            None => Err(se)?,
        }

        Ok(et.to_vec())
    }

    /// "+" | "-" | "or"
    fn additive_operator(&mut self) -> SyntaxResult {
        let et = [Kind::Plus, Kind::Minus, Kind::Or];
        let mut se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
        let err = || self.syntax_error([Kind::Plus, Kind::Minus, Kind::Or].as_ref(), &[]);
        match self.lookahead {
            Some(ref l) => match l.kind {
                Kind::Plus => self.match_consume_token(Kind::Plus),
                Kind::Minus => self.match_consume_token(Kind::Minus),
                Kind::Or => self.match_consume_token(Kind::Or),
                _ => Err(se)?,
            },
            None => Err(se)?,
        
        }

        Ok(et.to_vec())
    }

    /// "=" | "<>" | "<" | "<=" | ">" | ">="
    fn relational_operator(&mut self) -> SyntaxResult {
        let et = [Kind::Equal, Kind::NotEq, Kind::Less, Kind::LessEq, Kind::Great, Kind::GreatEq];
        let mut se = SyntaxError::new(self, et.to_vec(), self.lookahead.clone());
        let err = || {
            self.syntax_error(
                &et,
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
                _ => Err(se)?,
            },
            None => Err(se)?,
        }

        Ok(et.to_vec())
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
