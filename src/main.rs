use std::io;

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
        match parser::parse_expression(&line) {
            Ok((remaining, expr)) => println!("{:?} (remaining: {})", expr, remaining),
            Err(e) => eprintln!("{}", e),
        }
        line.clear();
    }
}
