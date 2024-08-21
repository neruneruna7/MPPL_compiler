mod scan1;
mod scan2;
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
