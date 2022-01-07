use std::io;

use structopt::StructOpt;

mod execute;
mod ir_tree;
mod parser;
mod syntax;

#[derive(StructOpt)]
#[structopt(name = "mlrs", about = "A small ML-like langauge written in Rust")]
struct Options {
    #[structopt(short, long)]
    debug: bool,
}

fn main() {
    let opts = Options::from_args();

    let stdin = io::stdin();
    let mut line = Default::default();

    let mut ir_mod = ir_tree::Module::new();
    let mut exec_context = execute::ExecContext::new();

    while let Ok(byte_count) = stdin.read_line(&mut line) {
        if byte_count == 0 {
            break;
        }

        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                if opts.debug {
                    println!("{:?} (remaining: {:?})", expr, remaining);
                }
                line.clear();
                expr
            }
            Err(e) => {
                eprintln!("{}", e);
                line.clear();
                continue;
            }
        };

        let new_root = ir_mod.add_expr(&expr);
        ir_mod.set_root(new_root);

        let result = exec_context.execute(&ir_mod);

        if let Some(result) = result {
            if opts.debug {
                println!("{:?}", ir_mod);
                println!("{:?}", result);
            } else {
                println!("{}", result);
            }
        }
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

        let ir_mod = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let mut exec_context = execute::ExecContext::new();
        let result = exec_context.execute(&ir_mod);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(15)));
    }

    #[test]
    fn partial_application() {
        let line = "f = (\\x -> \\y -> x + y) 5";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let mut ir_mod = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let mut exec_context = execute::ExecContext::new();
        let result = exec_context.execute(&ir_mod);
        println!("{:?}", result);
        assert_eq!(result, None);

        let line = "f 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let new_root = ir_mod.add_expr(&expr);
        ir_mod.set_root(new_root);

        let result = exec_context.execute(&ir_mod);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(15)));
    }

    #[test]
    fn normal_functions_and_multiplication() {
        let line = "double x = x * 2";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };

        let mut ir_mod = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let mut exec_context = execute::ExecContext::new();
        let result = exec_context.execute(&ir_mod);
        println!("{:?}", result);
        assert_eq!(result, None);

        let line = "double 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let new_root = ir_mod.add_expr(&expr);
        ir_mod.set_root(new_root);

        let result = exec_context.execute(&ir_mod);
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

        let mut ir_mod = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let mut exec_context = execute::ExecContext::new();
        let result = exec_context.execute(&ir_mod);
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
        let new_root = ir_mod.add_expr(&expr);
        ir_mod.set_root(new_root);

        let result = exec_context.execute(&ir_mod);
        println!("{:?}", result);
        assert_eq!(result, Some(Literal::Integer(10)));
    }
}
