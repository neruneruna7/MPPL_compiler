use super::SyntaxKind;

use crate::scan::scan3::Kind;

pub(crate) struct Node {
    pub(crate) kind: Kind,
    pub(crate) children: Vec<Node>,
}

pub(crate) enum NodeKind {
    Token(Kind),
    Syntax(SyntaxKind),
}
