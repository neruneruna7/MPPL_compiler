use super::SyntaxKind;

use crate::scan::scan3::Token;

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub children: Option<Vec<Node>>,
}

impl Node {
    pub(crate) fn new(kind: NodeKind, children: Option<Vec<Node>>) -> Self {
        Self { kind, children }
    }
}

#[derive(Debug, Clone)]
pub enum NodeKind {
    Token(Token),
    Syntax(SyntaxKind),
}
