use scan::scan3;

mod scan;
mod parser1 {
    use crate::scan::scan3::{Lexer, Token};

    pub struct Parser<'a> {
        source: &'a str,
        lexer: Lexer<'a>,
        cur_token: Token,
        prev_token_end: usize,
    }

    // impl<'a> Parser<'a> {
    // pub fn new(source: &'a str) -> Self {
    //     Self { source, lexer, cur_token, prev_token_end }
    // }
    // }
}

fn main() {
    println!("Hello, world!");
    let source = "
    if + {comment} if /* comment */
    a + b
    1+1
    if true then
        1 + 1
    else
        4 - 3
    ";

    let source = "
    if true then
    if if if if if if
    c c c c c c c c 
    { this is a comment }
        1 + 1
    else
        i := 5
    ";

    let mut lexer = scan3::Lexer::new(source);
    let tokens = lexer.analyze();

    // for i in tokens.iter() {
    //     println!("{:?}", i);
    // }

    // 字句の出現数をカウント
    let mut count = std::collections::HashMap::new();
    for token in tokens.into_iter() {
        let token = (token.kind, token.value);
        let entry = count.entry(token).or_insert(0);
        *entry += 1;
    }

    for (token, count) in count.iter() {
        println!("{:?} : {}", token, count);
    }
}
