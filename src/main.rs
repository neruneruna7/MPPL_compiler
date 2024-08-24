use prac_compiler::scan::scan3;

pub mod parser;

fn main() {
    let source = "
    program sample; if .";

    let lexer = scan3::Lexer::new(source);
    let mut parser = parser::parser1_ll1::Parser::new(lexer);
    parser.parse_program();

    println!("parsing OK \n{}", source);
}
