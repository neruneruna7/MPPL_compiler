use super::SyntaxKind;

use crate::scan::scan3::Kind;

pub(crate) struct Node {
    pub(crate) kind: NodeKind,
    pub(crate) children: Option<Vec<Node>>,
}

impl Node {
    pub(crate) fn new(kind: NodeKind, children: Option<Vec<Node>>) -> Self {
        Self { kind, children }
    }
}

pub(crate) enum NodeKind {
    Token(Kind),
    Syntax(SyntaxKind),
}
