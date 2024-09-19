
mod lexer;

struct grammar {
    rules: Vec<production_rule>, // Replace with set
    // Get terms or non terms?
}

struct production_rule {
    left: nonterminal,
    right: Option<Vec<symbol>>,
}

enum symbol {
    terminal,
    nonterm,
}

struct termial {
    value: String,
}

struct nonterminal {
    identifier: String,
}
fn main() {
    let nonterm_A: nonterminal = nonterminal {identifier: "A".to_string()};

    let mut test = grammar {rules: vec![
        production_rule {left: nonterm_A, right: None},
    ]};

}