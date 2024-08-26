use prac_compiler::scan::scan3;

pub mod parser;

const TEST_SOURCE_COUNT: usize = 1;

fn main() {
    // ./test_source/perse/1.mpl を読み込む
    //  定数で宣言した数だけファイルが存在するので，すべて読み込む
    // let source = include_str!("../test_source/perse/1.mpl");

    for i in 1..=TEST_SOURCE_COUNT {
        // includeではなくファイルオープンを使って
        // ファイルを読み込む
        let source = std::fs::read_to_string(format!("test_source/perse/{}.mpl", i)).unwrap();
        let lexer = scan3::Lexer::new(&source);
        let mut parser = parser::parser1_ll1::Parser::new(lexer);
        parser.parse_program();

        println!("parsing OK \n{}", source);
    }
}
