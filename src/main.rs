mod automaton_scan;
mod simple_scan;
mod scan3;

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
    { this is a comment }
        1 + 1
    else
        i := 5
    ";

    let mut lexer = scan3::Lexer::new(source);
    let tokens = lexer.analyze();

    for i in tokens.iter() {
        println!("{:?}", i);
    }
}
