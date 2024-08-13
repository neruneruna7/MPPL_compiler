mod automaton_scan;
mod simple_scan;

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
    {comment} /* comment */
    ";


    let mut lexer = automaton_scan::Lexer::new(source);
    let tokens = lexer.analyze();

    for i in tokens.iter() {
        println!("{:?}", i);
    }
}
