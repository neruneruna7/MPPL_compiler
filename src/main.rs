use parser::parser2_ll1::Parser;
use prac_compiler::scan::scan3;

pub mod parser;

const TEST_SOURCE_COUNT: usize = 1;

fn main() {
    for i in 1..=TEST_SOURCE_COUNT {
        let source = std::fs::read_to_string(format!("test_source/perse/{}.mppl", i)).unwrap();
        let lexer = scan3::Lexer::new(&source);
        let mut parser = Parser::new(lexer);
        parser.parse_program().unwrap();

        println!("{} parsing OK \n{}", i, source);
    }
}
