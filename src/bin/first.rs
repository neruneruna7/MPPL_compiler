// EBNFからFIRST集合を求めるプログラム
// []は1回または0回
// {}は0回以上の繰り返し
// |はまたは
// ->は導出
// ::=は定義
// EBNFの例
// <A> ::= <B> | <C> | <D>
// <B> ::= "b"
// <C> ::= [ "c" ] "b"
// <D> ::= { "d" } "b"
use std::{
    collections::{HashMap, HashSet},
    sync::OnceLock,
};

#[derive(Debug, Clone)]
struct Rule {
    left: String,
    right: String,
}

struct FirstSet {
    first: Vec<String>,
}

fn main() {
    let rules = [
        Rule {
            left: "A".to_string(),
            right: "B".to_string(),
        },
        Rule {
            left: "A".to_string(),
            right: "C".to_string(),
        },
        Rule {
            left: "A".to_string(),
            right: "D".to_string(),
        },
        Rule {
            left: "B".to_string(),
            right: "d".to_string(),
        },
        Rule {
            left: "C".to_string(),
            right: "[ c ] b".to_string(),
        },
        Rule {
            left: "D".to_string(),
            right: "{ d } b".to_string(),
        },
    ];

    let first_set = calc_first_set(rules.to_vec());
    for (k, v) in first_set.iter() {
        println!("FIRST({}) = {:?}", k, v);
    }
}

// Once_cellで1度だけ書き換え可能な文法規則のベクタ
static RULES: OnceLock<Vec<Rule>> = OnceLock::new();

fn calc_first_set(rules: Vec<Rule>) -> HashMap<String, HashSet<String>> {
    // rulesをOnce_cellでグローバル変数に代入する
    RULES.get_or_init(|| rules);

    let mut first_set = HashMap::new();
    // 文法規則すべてについて，FIRST集合を求める
    for r in RULES.get().unwrap().iter() {
        // 空白で区切る
        let left = r.left.split_whitespace().collect::<Vec<&str>>();

        let set = first(&left);
        first_set.insert(r.left.clone(), set);
    }

    first_set
}

fn first(a: &[&str]) -> HashSet<String> {
    // 0 FIRST(a)を空集合に初期化
    let mut first_a = HashSet::new();

    // 以下のステップを，付け加えるものがなくなるまで繰り返す
    loop {
        let before = first_a.clone();
        match a {
            // 1 aが空系列ならFIRST(a)に空系列を追加
            ["ε"] => {
                first_a.insert("ε".to_string());
            }
            // 2 aが終端記号1文字なら，FIRST(a)にその終端記号を追加
            _ if a.len() == 1 && is_terminal(&a[0]) => {
                first_a.insert(a[0].to_string());
            }
            // 3 aが非終端記号A1文字なら，Aを左辺にもつすべての生成規則についてのFIRST集合を求め，
            // その要素をFIRST(A)に付け加える
            _ if a.len() == 1 && !is_terminal(&a[0]) => {
                let a = a[0];
                let rules = RULES.get().unwrap();
                for r in rules.iter() {
                    if r.left == a {
                        let right = r.right.split_whitespace().collect::<Vec<&str>>();
                        let set = first(&right);
                        first_a.extend(set);
                    }
                }
            }
            // 5 aが b | y の場合 FIRST(b)とFIRST(y)をFIRST(a)に付け加える
            _ if matches!(is_pattern5(a), Some(b)) => {
                let (b, y) = is_pattern5(a).unwrap();
                let set_b = first(&b);
                let set_y = first(&y);
                first_a.extend(set_b);
                first_a.extend(set_y);
            }
            // 6 aが{ b } の場合 FIRST(b)とεをFIRST(a)に付け加える
            _ if matches!(is_pattern6(a), Some(b)) => {
                // matches! でとった変数のスコープは，ここにまでは持ってこれないっぽい？
                // ので，仕方なくここでもう一度関数を呼ぶ
                let b = is_pattern6(a).unwrap();
                let set_b = first(&b);
                first_a.extend(set_b);
                first_a.insert("ε".to_string());
            }
            // 7 aが[ b ] の場合 FIRST(b)とεをFIRST(a)に付け加える
            _ if matches!(is_pattern7(a), Some(b)) => {
                let b = is_pattern7(a).unwrap();
                let set_b = first(&b);
                first_a.extend(set_b);
                first_a.insert("ε".to_string());
            }
            // 8 aが( b )の場合 FIRST(b)をFIRST(a)に付け加える
            _ if matches!(is_pattern8(a), Some(b)) => {
                let b = is_pattern8(a).unwrap();
                let set_b = first(&b);
                first_a.extend(set_b);
            }
            // 4 aが長さ2以上の系列の場合(a=Xbとする)
            _ if a.len() >= 2 => {
                // 4-1 FIRST(X)がεを含まないなら，FIRST(X)をFIRST(a)に付け加える
                let x = a[0];
                let mut set_x = first(&[x]);
                if !set_x.contains("ε") {
                    first_a.extend(set_x);
                }
                // 4-1 FIRST(X)がεを含むなら，FIRST(X)からεを取り除いたものとFIRST(b)をFIRST(a)に付け加える
                else {
                    set_x.remove("ε");
                    first_a.extend(set_x);
                    let b = a[1];
                    let set_b = first(&[b]);
                    first_a.extend(set_b);
                }
            }

            _ => {
                eprintln!("error first set fn");
            }
        }

        // 付け加えるものがなくなったら終了
        if before == first_a {
            break;
        }
    }

    first_a
}

// とにかく，はじめの|の部分とそれ以降の部分に分ける
// そうすれば，あとは再帰的に解決できるか
fn is_pattern5<'a>(a: &[&'a str]) -> Option<(Vec<&'a str>, Vec<&'a str>)> {
    // | が含まれてないならその時点でfalse
    if !a.iter().any(|x| x.contains('|')) {
        return None;
    }
    let mut symbol_count = HashMap::new();
    symbol_count.insert("(", 0);
    symbol_count.insert("[", 0);
    symbol_count.insert("{", 0);

    let mut left = Vec::new();
    for i in a.iter() {
        match *i {
            "(" => {
                let c = symbol_count.get_mut(&"(").unwrap();
                *c += 1;
            }
            ")" => {
                let c = symbol_count.get_mut(&"(").unwrap();
                *c -= 1;
            }
            "{" => {
                let c = symbol_count.get_mut(&"{").unwrap();
                *c += 1;
            }
            "}" => {
                let c = symbol_count.get_mut(&"{").unwrap();
                *c -= 1;
            }
            "[" => {
                let c = symbol_count.get_mut(&"[").unwrap();
                *c += 1;
            }
            "]" => {
                let c = symbol_count.get_mut(&"[").unwrap();
                *c -= 1;
            }
            "|" => {
                let right = a[left.len()..].as_ref();
                return Some((left, right.to_vec()));
            }
            _ => {
                unreachable!()
            }
        }
        left.push(*i);
    }
    // ( { a } | b | [ c ] ) { a }
    todo!()
}

fn is_pattern6<'a>(a: &[&'a str]) -> Option<Vec<&'a str>> {
    if let Some(s) = a.iter().nth(0) {
        if *s == "{" {
            let mut buf = Vec::new();
            let mut itr = a.iter();
            itr.next().unwrap();
            for i in itr {
                if *i == "}" {
                    break;
                }
                buf.push(*i)
            }
            return Some(buf);
        }
    }
    None
}

fn is_pattern7<'a>(a: &[&'a str]) -> Option<Vec<&'a str>> {
    if let Some(s) = a.iter().nth(0) {
        if *s == "[" {
            let mut buf = Vec::new();
            let mut itr = a.iter();
            itr.next().unwrap();
            for i in itr {
                if *i == "]" {
                    break;
                }
                buf.push(*i)
            }
            return Some(buf);
        }
    }
    None
}

fn is_pattern8<'a>(a: &[&'a str]) -> Option<Vec<&'a str>> {
    if let Some(s) = a.iter().nth(0) {
        if *s == "(" {
            let mut buf = Vec::new();
            let mut itr = a.iter();
            itr.next().unwrap();
            for i in itr {
                if *i == ")" {
                    break;
                }
                buf.push(*i)
            }
            return Some(buf);
        }
    }
    None
}

// 文字列が終端記号かどうか
// すべて小文字なら終端記号
// キャメルケースは非終端記号
fn is_terminal(s: &str) -> bool {
    s.chars().all(|c| c.is_lowercase())
}
