use std::path::{Path, PathBuf};
use std::{
    fs::{self, File},
    io::{self, Write},
};

use clap::{ArgGroup, Parser};

mod execute;
use execute::evaluate_id;
mod ir_tree;
mod parser;
mod syntax;
mod utils;

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

    /// Dump path for debug info
    #[structopt(long)]
    debug_file: Option<PathBuf>,
}

enum DebugPrinter {
    DoNotPrint,
    PrintStdOut,
    PrintToFile(File),
}

impl Write for DebugPrinter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        match self {
            DebugPrinter::DoNotPrint => Ok(0),
            DebugPrinter::PrintStdOut => {
                let mut stdout = std::io::stdout();
                stdout.write(buf)
            }
            DebugPrinter::PrintToFile(f) => f.write(buf),
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        match self {
            DebugPrinter::DoNotPrint => Ok(()),
            DebugPrinter::PrintStdOut => std::io::stdout().flush(),
            DebugPrinter::PrintToFile(f) => f.flush(),
        }
    }
}

fn add_decl_or_expr(
    ir_mod: &mut ir_tree::Module,
    decl_or_expr: parser::DeclOrExpr,
    debug: &mut DebugPrinter,
) {
    use parser::DeclOrExpr::*;

    let (lowering_result, should_eval) = match decl_or_expr {
        Decl(decl) => (ir_mod.add_decl(&decl), false),
        Expr(expr) => (ir_mod.add_expr(&expr), true),
    };

    match lowering_result {
        Ok(expr_id) => {
            writeln!(debug, "{:?}", ir_mod).unwrap();

            if should_eval {
                print_eval_of(ir_mod, expr_id, debug);
            }
        }
        Err(e) => eprintln!("{}", e),
    }
}

fn ir_from_file(filename: &Path, debug: &mut DebugPrinter) -> ir_tree::Module {
    let file_data = match fs::read_to_string(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error loading {:?}: {}", filename, e);
            Default::default()
        }
    };

    let mut ir_mod = ir_tree::Module::new();
    match parser::parse_whole_file(&file_data) {
        Ok((remaining, items)) => {
            writeln!(debug, "{:?} (remaining: {:?})", items, remaining).unwrap();

            for decl_or_expr in items {
                add_decl_or_expr(&mut ir_mod, decl_or_expr, debug);
            }
        }
        Err(e) => {
            eprintln!("{}", e);
        }
    }
    ir_mod
}

fn interact_with(input: io::Stdin, mut ir_mod: ir_tree::Module, debug: &mut DebugPrinter) {
    let mut line = Default::default();
    while let Ok(byte_count) = input.read_line(&mut line) {
        if byte_count == 0 {
            break;
        }

        let decl_or_expr = match parser::parse_decl_or_expr(&line) {
            Ok((remaining, item)) => {
                writeln!(debug, "{:?} (remaining: {:?})", item, remaining).unwrap();
                line.clear();
                item
            }
            Err(e) => {
                eprintln!("{}", e);
                line.clear();
                continue;
            }
        };

        let is_expr = matches!(decl_or_expr, parser::DeclOrExpr::Expr(_));

        let new_id = match decl_or_expr {
            parser::DeclOrExpr::Expr(expr) => ir_mod.add_expr(&expr),
            parser::DeclOrExpr::Decl(decl) => ir_mod.add_decl(&decl),
        };

        if is_expr {
            match new_id {
                Ok(new_id) => print_eval_of(&ir_mod, new_id, debug),
                Err(e) => eprintln!("{}", e),
            }
        }
    }
}

fn print_eval_of(module: &ir_tree::Module, id: ir_tree::IRId, debug: &mut DebugPrinter) {
    writeln!(debug, "{:?}", module).unwrap();

    println!("{}", evaluate_id(module, id));
}

fn main() {
    let opts = Options::parse();

    let mut debug_printer = match (opts.debug, opts.debug_file) {
        (true, None) => DebugPrinter::PrintStdOut,
        (true, Some(path)) => {
            DebugPrinter::PrintToFile(File::create(path).expect("Could not open debug file"))
        }
        (false, None) => DebugPrinter::DoNotPrint,
        (false, Some(path)) => {
            eprintln!(
                "Specified a path (namely {}), but `--debug` was false",
                path.to_string_lossy()
            );
            return;
        }
    };

    let ir_mod = opts
        .input_file
        .map(|filename| ir_from_file(filename.as_path(), &mut debug_printer))
        .unwrap_or_default();

    if opts.interactive {
        interact_with(io::stdin(), ir_mod, &mut debug_printer);
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::execute::EvaluationResult;
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

        let (ir_mod, expr_id) = ir_tree::Module::from_expr(&expr).unwrap();
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(15)));
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

        let mut ir_mod = ir_tree::Module::from_decls(&[decl]).unwrap();
        println!("{:?}", ir_mod);

        let line = "f 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr).unwrap();

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(15)));
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

        let mut ir_mod = ir_tree::Module::from_decls(&[decl]).unwrap();
        println!("{:?}", ir_mod);

        let line = "double 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr).unwrap();

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(20)));
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

        let (mut ir_mod, expr_id) = ir_tree::Module::from_expr(&expr).unwrap();
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(5)));

        let line = "if false then 5 else 10";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, expr)) => {
                println!("{:?} (remaining: {:?})", expr, remaining);
                expr
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr).unwrap();

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(10)));
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

        let (mut ir_mod, expr_id) = ir_tree::Module::from_expr(&expr).unwrap();
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(10)));

        let line = "x = 10";
        let decl = match parser::parse_declaration(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };
        ir_mod.add_decl(&decl).unwrap();

        let expr = match parser::parse_expression("x") {
            Ok((_, expr)) => expr,
            Err(e) => panic!("{}", e),
        };

        let expr_id = ir_mod.add_expr(&expr).unwrap();

        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(10)));
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

        let mut ir_mod = ir_tree::Module::from_decls(&[func]).unwrap();

        let line = "plusTen 3";
        let expr = match parser::parse_expression(&line) {
            Ok((remaining, decl)) => {
                println!("{:?} (remaining: {:?})", decl, remaining);
                decl
            }
            Err(e) => panic!("{}", e),
        };
        let expr_id = ir_mod.add_expr(&expr).unwrap();
        println!("{:?}", ir_mod);

        let result = evaluate_id(&ir_mod, expr_id);
        println!("{:?}", result);
        assert_eq!(result, EvaluationResult::Literal(Literal::Integer(13)));
    }
}
