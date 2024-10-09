mod grammars;
use grammars::common;

mod cnf {
    use crate::common::{Constant, Variable};

    pub struct Grammar {
        production_rules: Vec<Production>,
        start: Variable,
    }

    impl Grammar {
        pub fn nodes_from_constant(&self, value: &Constant) -> Vec<Node> {
            let terminal_rules = self.production_rules.iter().filter(|x| {
                if let Rhs::Leaf(l) = &x.rhs {
                    *l == *value
                } else {
                    false
                }
            });

            let mut node_values: Vec<Node> = Vec::new();

            for rule in terminal_rules {
                node_values.push(Node {
                    variable: rule.variable.clone(),
                    rhs: Rhs::Leaf(value.clone()),
                });
            }

            node_values
        }
    }

    #[derive(Debug)]
    pub struct Rule<T> {
        variable: Variable,
        rhs: Rhs<T>,
    }

    #[derive(Debug)]
    pub enum Rhs<T> {
        Branch(WrappedVariable<T>, WrappedVariable<T>),
        Leaf(Constant),
    }

    #[derive(Debug)]
    pub struct WrappedVariable<T> {
        variable: Variable,
        augment: T,
    }

    pub type Production = Rule<()>;
    #[derive(Debug)]
    pub struct NodeInner(Box<Node>);
    pub type Node = Rule<NodeInner>;

    pub fn cyk_parse(input: Vec<Constant>, grammar: Grammar) -> Vec<Vec<Vec<Node>>> {
        let tree: Option<Node> = None;
        let len: usize = input.len();

        type Nodes = Vec<Node>; // Vector of previously found nodes

        let mut chart: Vec<Vec<Nodes>> = Vec::new();

        for width in 0..len {
            chart.push(Vec::new());

            for height in 0..(len - width) {
                chart[width].push(Vec::new());
                chart[width][height] = Vec::new();
            }

            chart[width][0] = grammar.nodes_from_constant(&input[width]);
        }

        chart
    }

    #[cfg(test)]
    mod cnf_tests {
        use super::*;

        fn nonterm_production(variable: &str, left: &str, right: &str) -> Production {
            Production {
                variable: Variable(variable.to_string()),
                rhs: Rhs::Branch(
                    WrappedVariable {
                        variable: Variable(left.to_string()),
                        augment: (),
                    },
                    WrappedVariable {
                        variable: Variable(right.to_string()),
                        augment: (),
                    },
                ),
            }
        }

        fn term_production(variable: &str, constant: &str) -> Production {
            Production {
                variable: Variable(variable.to_string()),
                rhs: Rhs::Leaf(Constant(constant.to_string())),
            }
        }

        fn test_grammar() -> Grammar {
            Grammar {
                production_rules: vec![
                    nonterm_production("S", "A", "B"), // S -> AB
                    term_production("S", ""), // S -> e
                    nonterm_production("A", "B", "B"), // A -> BB
                    term_production("A", "a"), // A -> a
                    term_production("B", "b"), // B -> b
                ],
                start: Variable(String::from("S")),
            }
        }

        fn test_input() -> Vec<Constant> {
            let input_string: &str = "aaaabbbbbb";
            let v: Vec<Constant> = input_string.chars().map(|val| Constant(val.to_string())).collect();
            v
        }

        #[test]
        fn const_to_node() {
            let g = test_grammar();
            let input = test_input();
            let cyk = cyk_parse(input, g);

            panic!("{:?}", cyk);

        }
    }
}

fn main() {
    let abc = "abc";
}

#[cfg(test)]
mod grammar_test {
    use super::*;

    #[test]
    fn array_test() {}
}
