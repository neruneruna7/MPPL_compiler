use std::cell::LazyCell;

use crate::scan::scan3::Kind;

use std::collections::HashSet;

use super::SyntaxKind;

pub(crate) struct FirstSet {
    pub(crate) symbol: SyntaxKind,
    pub(crate) first_set: HashSet<Kind>,
}

impl FirstSet {
    pub(crate) fn new(symbol: SyntaxKind, first_set: HashSet<Kind>) -> Self {
        Self { symbol, first_set }
    }
}

pub(crate) const FIRST_SETS: LazyCell<Vec<FirstSet>> = {
    LazyCell::new(|| {
        vec![
            FirstSet::new(
                SyntaxKind::Program,
                vec![Kind::Program].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::Block,
                vec![Kind::Var, Kind::Procedure, Kind::Begin]
                    .into_iter()
                    .collect(),
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
            FirstSet::new(
                SyntaxKind::Type,
                vec![Kind::Integer, Kind::Boolean, Kind::Char, Kind::Array]
                    .into_iter()
                    .collect(),
            ),
            FirstSet::new(
                SyntaxKind::StandardType,
                vec![Kind::Integer, Kind::Boolean, Kind::Char]
                    .into_iter()
                    .collect(),
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
                    Kind::Semicolon,
                    Kind::End,
                    Kind::Else,
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
                vec![
                    Kind::Name,
                    Kind::Plus,
                    Kind::Minus,
                    Kind::LParen,
                    Kind::Not,
                    Kind::Integer,
                    Kind::True,
                    Kind::False,
                    Kind::String,
                    Kind::Boolean,
                    Kind::Char,
                    Kind::UnsignedInteger,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::ReturnStatement,
                vec![Kind::Return].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::AssignmentStatement,
                vec![Kind::Name].into_iter().collect(),
            ),
            FirstSet::new(SyntaxKind::LeftPart, vec![Kind::Name].into_iter().collect()),
            FirstSet::new(SyntaxKind::Variable, vec![Kind::Name].into_iter().collect()),
            FirstSet::new(
                SyntaxKind::Expression,
                vec![
                    Kind::Name,
                    Kind::Plus,
                    Kind::Minus,
                    Kind::LParen,
                    Kind::Not,
                    Kind::Integer,
                    Kind::True,
                    Kind::False,
                    Kind::String,
                    Kind::Boolean,
                    Kind::Char,
                    Kind::UnsignedInteger,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::SimpleExpression,
                vec![
                    Kind::Name,
                    Kind::Plus,
                    Kind::Minus,
                    Kind::LParen,
                    Kind::Not,
                    Kind::Integer,
                    Kind::True,
                    Kind::False,
                    Kind::String,
                    Kind::Boolean,
                    Kind::Char,
                    Kind::UnsignedInteger,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::Term,
                vec![
                    Kind::Name,
                    Kind::Plus,
                    Kind::Minus,
                    Kind::LParen,
                    Kind::Not,
                    Kind::Integer,
                    Kind::True,
                    Kind::False,
                    Kind::String,
                    Kind::Boolean,
                    Kind::Char,
                    Kind::UnsignedInteger,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::Factor,
                vec![
                    Kind::Name,
                    Kind::LParen,
                    Kind::Not,
                    Kind::Integer,
                    Kind::True,
                    Kind::False,
                    Kind::String,
                    Kind::Boolean,
                    Kind::Char,
                    Kind::UnsignedInteger,
                ]
                .into_iter()
                .collect(),
            ),
            FirstSet::new(
                SyntaxKind::Constant,
                vec![Kind::UnsignedInteger, Kind::True, Kind::False, Kind::String]
                    .into_iter()
                    .collect(),
            ),
            FirstSet::new(
                SyntaxKind::MultiplicativeOperator,
                vec![Kind::Star, Kind::Div, Kind::And].into_iter().collect(),
            ),
            FirstSet::new(
                SyntaxKind::AdditiveOperator,
                vec![Kind::Plus, Kind::Minus, Kind::Or]
                    .into_iter()
                    .collect(),
            ),
            FirstSet::new(
                SyntaxKind::RelationalOperator,
                vec![
                    Kind::Equal,
                    Kind::NotEq,
                    Kind::Less,
                    Kind::LessEq,
                    Kind::Great,
                    Kind::GreatEq,
                ]
                .into_iter()
                .collect(),
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
                vec![
                    Kind::True,
                    Kind::Not,
                    Kind::Plus,
                    Kind::Boolean,
                    Kind::Minus,
                    Kind::False,
                    Kind::LParen,
                    Kind::Char,
                    Kind::UnsignedInteger,
                    Kind::String,
                    Kind::Name,
                    Kind::Integer,
                ]
                .into_iter()
                .collect(),
            ),
        ]
    })
};
