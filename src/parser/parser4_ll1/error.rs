use super::first_set::FIRST_SETS;

use super::Parser;

use crate::scan::scan3::Kind;
use crate::scan::scan3::{self, Token};

use super::SyntaxKind;

// 独自のエラー型を定義
// どんなトークンを期待していたが，実際にはどんなトークンが来たかを表現する
#[derive(Debug)]
pub struct SyntaxError {
    pub(crate) lexeicalized_source: String,
    pub(crate) expected_token: Vec<scan3::Kind>,
    pub(crate) expected_syntax: Vec<SyntaxKind>,
    pub(crate) found: Token,
}

impl SyntaxError {
    pub fn new(
        parser: &Parser,
        expected_token: &[scan3::Kind],
        expected_syntax: &[SyntaxKind],
    ) -> Self {
        let sliced_source = &parser.lexer.source[..parser.lookahead.start];

        let expected_token = expected_token.to_vec();
        let expected_syntax = expected_syntax.to_vec();
        let found = parser.lookahead.clone();
        Self {
            lexeicalized_source: sliced_source.to_string(),
            expected_token,
            expected_syntax,
            found,
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
        let found = format!("{:?}", self.found);

        let binding = FIRST_SETS;
        let tokens = binding
            .iter()
            .filter(|x| self.expected_syntax.contains(&x.symbol))
            .flat_map(|x| x.first_set.iter().collect::<Vec<&Kind>>())
            .collect::<Vec<&Kind>>();

        write!(
            f,
            "source code:\n\n {} \n\n expect token= {:?}, syntax= {:?} token= {:?} but found {:?}",
            self.lexeicalized_source, expected_token, self.expected_syntax, tokens, found
        )
    }
}

impl std::error::Error for SyntaxError {}
