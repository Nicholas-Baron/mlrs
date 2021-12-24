use std::io;

mod execute;
mod ir_tree;
mod parser;
mod syntax;

fn main() {
    let stdin = io::stdin();
    let mut line = Default::default();

    while let Ok(byte_count) = stdin.read_line(&mut line) {
        if byte_count == 0 {
            break;
        }

        println!("{}", line);
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                line.clear();
                expr
            }
            Err(e) => {
                eprintln!("{}", e);
                line.clear();
                continue;
            }
        };

        let ir_mod = ir_tree::Module::from_expr(&expr);
        println!("{:?}", ir_mod);

        let mut exec_context = execute::ExecContext::new(ir_mod);
        let result = exec_context.execute();
        println!("{:?}", result);
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

        let mut exec_context = execute::ExecContext::new(ir_mod);
        let result = exec_context.execute();
        println!("{:?}", result);
        assert_eq!(result, Literal::Integer(15));
    }
}
