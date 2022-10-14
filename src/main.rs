use std::path::PathBuf;
use std::{fs, io};

use clap::{ArgGroup, Parser};

mod execute;
use execute::evaluate_id;
mod ir_tree;
mod parser;
mod syntax;

#[derive(Parser)]
#[structopt(name = "mlrs", about = "A small ML-like langauge written in Rust")]
#[command(group(
            ArgGroup::new("input")
                .required(true)
                .multiple(true)
                .args(["interactive", "input_file"]),
        ))]
struct Options {
    #[structopt(short, long)]
    debug: bool,

    /// Input file
    input_file: Option<PathBuf>,

    /// Interactive mode
    #[structopt(short, long)]
    interactive: bool,
}

fn ir_from_file(filename: &str, debug: bool) -> ir_tree::Module {
    let file_data = match fs::read_to_string(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error loading {:?}: {}", filename, e);
            Default::default()
        }
    };

    let mut ir_mod = ir_tree::Module::new();
    match parser::parse_decl_or_expr_list(&file_data) {
        Ok((remaining, items)) => {
            if debug {
                println!("{:?} (remaining: {:?})", items, remaining);
            }

            for decl_or_expr in items {
                use parser::DeclOrExpr::*;
                match decl_or_expr {
                    Decl(decl) => {
                        ir_mod.add_decl(&decl);
                    }
                    Expr(expr) => {
                        let expr_id = ir_mod.add_expr(&expr);
                        print_result(evaluate_id(&ir_mod, expr_id), debug.then_some(&ir_mod));
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
    ir_mod
}

fn interact_with(input: io::Stdin, mut ir_mod: ir_tree::Module, debug: bool) {
    let mut line = Default::default();
    while let Ok(byte_count) = input.read_line(&mut line) {
        if byte_count == 0 {
            break;
        }

        let decl_or_expr = match parser::parse_decl_or_expr(&line) {
            Ok((remaining, item)) => {
                if debug {
                    println!("{:?} (remaining: {:?})", item, remaining);
                }
                line.clear();
                item
            }
            Err(e) => {
                eprintln!("{}", e);
                line.clear();
                continue;
            }
        };

        let new_id = match decl_or_expr {
            parser::DeclOrExpr::Expr(expr) => ir_mod.add_expr(&expr),
            parser::DeclOrExpr::Decl(decl) => ir_mod.add_decl(&decl),
        };

        print_result(evaluate_id(&ir_mod, new_id), debug.then_some(&ir_mod));
    }
}

fn print_result(result: Option<syntax::Literal>, ir_mod: Option<&ir_tree::Module>) {
    if let Some(result) = result {
        if let Some(ir_mod) = ir_mod {
            println!("{:?}", ir_mod);
            println!("{:?}", result);
        } else {
            println!("{}", result);
        }
    }
}

fn main() {
    let opts = Options::parse();

    let ir_mod = if let Some(ref filename) = opts.input_file {
        ir_from_file(filename.to_str().unwrap(), opts.debug)
    } else {
        Default::default()
    };

    if opts.interactive {
        interact_with(io::stdin(), ir_mod, opts.debug);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::syntax::Literal;

    #[test]
    fn basic_program_test() {
        let line = "(\\x -> \\y -> x + y) 5 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let (ir_mod, expr_id) = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(15)));
    }

    #[test]
    fn partial_application() {
        let line = "f = (\\x -> \\y -> x + y) 5";
        let decl = match parser::parse_declaration(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };

        let mut ir_mod = ir_tree::Module::from_decls(&[decl]);
        println!("{:?}", ir_mod);

        let line = "f 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(15)));
    }

    #[test]
    fn normal_functions_and_multiplication() {
        let line = "double x = x * 2";
        let decl = match parser::parse_declaration(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };

        let mut ir_mod = ir_tree::Module::from_decls(&[decl]);
        println!("{:?}", ir_mod);

        let line = "double 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(20)));
    }

    #[test]
    fn conditionalsd() {
        let line = "if true then 5 else 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let (mut ir_mod, expr_id) = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(5)));

        let line = "if false then 5 else 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(10)));
    }

    #[test]
    fn lets() {
        let line = "let x = 5 in x + 5";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let (mut ir_mod, expr_id) = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(10)));

        let line = "x = 10";
        let decl = match parser::parse_declaration(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };
        let decl_id = ir_mod.add_decl(&decl);

        let result = evaluate_id(&ir_mod, decl_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(10)));
    }

    #[test]
    fn subfunctions() {
        let input = r#"
          plusTen x = let
            plusFive x = x + 5
          in plusFive (plusFive x)
        "#;

        let func = match parser::parse_declaration(&input) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let mut ir_mod = ir_tree::Module::from_decls(&[func]);

        let line = "plusTen 3";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr);
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(13)));
    }
}
