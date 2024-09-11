use crate::scan::scan3::Kind;
use crate::parser::parser4_ll1::SyntaxKind;

struct Node {
    kind: Kind,
    children: Vec<Node>,
}

enum NodeKind {
    Token(Kind),
    Syntax(SyntaxKind),
}