use super::syntax::*;

use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{self, char, multispace0, multispace1, space0},
    combinator, multi, sequence, IResult,
};

mod expression;
pub use expression::parse_expression;

fn parse_separator(input: &str) -> IResult<&str, ()> {
    combinator::value(
        (),
        multi::many1_count(sequence::preceded(
            space0,
            branch::alt((complete::line_ending, tag(";"))),
        )),
    )(input)
}

const RESERVED_WORDS: &[&str] = &[
    "if", "then", "else", "true", "True", "false", "False", "let", "in",
];

fn parse_identifier(input: &str) -> IResult<&str, String> {
    use complete::{alpha1, alphanumeric0};
    combinator::verify(
        combinator::map(sequence::pair(alpha1, alphanumeric0), |(first, second)| {
            format!("{}{}", first, second)
        }),
        |id: &String| !RESERVED_WORDS.into_iter().any(|kw| kw == id),
    )(input)
}

fn parse_comment(input: &str) -> IResult<&str, ()> {
    combinator::value(
        (),
        sequence::tuple((
            branch::alt((tag("#"), tag("//"), tag("--"))),
            complete::not_line_ending,
            complete::line_ending,
        )),
    )(input)
}

#[derive(Debug)]
pub enum DeclOrExpr {
    Decl(Declaration),
    Expr(Expr),
}

pub fn parse_decl_or_expr(input: &str) -> IResult<&str, DeclOrExpr> {
    branch::alt((
        combinator::map(parse_declaration, DeclOrExpr::Decl),
        combinator::map(parse_expression, DeclOrExpr::Expr),
    ))(input)
}

pub fn parse_declaration_list(input: &str) -> IResult<&str, Vec<Declaration>> {
    multi::separated_list0(multi::many1(parse_separator), parse_declaration)(input)
}

pub fn parse_declaration(input: &str) -> IResult<&str, Declaration> {
    branch::alt((
        combinator::map(
            sequence::tuple((
                multispace0,
                parse_identifier,
                multispace0,
                char('='),
                multispace0,
                parse_expression,
            )),
            |(_, name, _, _eq, _, expr)| Declaration {
                name,
                expr: Box::new(expr),
            },
        ),
        combinator::map(
            sequence::tuple((
                multispace0,
                parse_identifier,
                multispace1,
                multi::separated_list1(multispace1, parse_identifier),
                multispace0,
                char('='),
                multispace0,
                parse_expression,
            )),
            |(_, name, _, args, _, _eq, _, expr)| Declaration {
                name,
                expr: args.into_iter().rev().fold(Box::new(expr), |body, arg| {
                    Box::new(Expr::Lambda {
                        body,
                        parameter: arg,
                    })
                }),
            },
        ),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_identifier_test() {
        assert_eq!(parse_identifier("x"), Ok(("", "x".to_string())));
        assert!(matches!(parse_identifier("123"), Err(_)));
        assert_eq!(parse_identifier("x12y"), Ok(("", "x12y".to_string())));
    }

    #[test]
    fn parse_binding_test() {
        assert_eq!(
            parse_declaration("x = 5"),
            Ok((
                "",
                Declaration {
                    name: "x".to_string(),
                    expr: Box::new(Expr::Literal(Literal::Integer(5)))
                }
            ))
        );

        assert_eq!(
            parse_declaration("x = (\\ a b -> a + b) 5"),
            Ok((
                "",
                Declaration {
                    name: "x".to_string(),
                    expr: Box::new(Expr::Binary {
                        op: BinaryOperation::Application,
                        rhs: Box::new(Expr::Literal(Literal::Integer(5))),
                        lhs: Box::new(Expr::Lambda {
                            parameter: "a".to_string(),
                            body: Box::new(Expr::Lambda {
                                parameter: "b".to_string(),
                                body: Box::new(Expr::Binary {
                                    op: BinaryOperation::Plus,
                                    lhs: Box::new(Expr::Identifier("a".to_string())),
                                    rhs: Box::new(Expr::Identifier("b".to_string())),
                                }),
                            }),
                        }),
                    })
                }
            ))
        );
    }

    #[test]
    fn parse_expressions_test() {
        let (rest, exprs) = parse_declaration_list("f x y = x + y\n\n double x = x * 2").unwrap();
        assert_eq!(rest.len(), 0);
        assert_eq!(exprs.len(), 2);
    }

    #[test]
    fn parse_comment_test() {
        assert_eq!(parse_expression("-- Hello World\n5"), parse_expression("5"));
        assert_eq!(parse_expression("// This is\n5"), parse_expression("5"));
        assert_eq!(parse_expression("# a comment\n5"), parse_expression("5"));
    }

    #[test]
    fn parse_function_binding_test() {
        assert_eq!(
            parse_declaration("f x y = x + y"),
            parse_declaration("f = \\x -> \\y -> x+y")
        );
    }

    #[test]
    fn parse_extra_newlines_test() {
        assert_eq!(
            parse_declaration("f x y = x + y"),
            parse_declaration("f x y\n    = x + y")
        );
        let (rest, decl) = parse_declaration(
            r"
fib x = if x == 0 then 0
   else if x == 1 then 1
   else fib (x - 1)
                ",
        )
        .unwrap();
        assert_eq!(rest.trim(), "");
        assert!(matches!(decl, Declaration { .. }));
    }

    #[test]
    fn parse_let_test() {
        assert_eq!(
            parse_expression("let x = 5 in x"),
            Ok((
                "",
                Expr::Let {
                    bound_values: vec![Declaration {
                        name: "x".to_string(),
                        expr: Box::new(Expr::Literal(Literal::Integer(5)))
                    }],
                    inner_expr: Box::new(Expr::Identifier("x".to_string()))
                }
            ))
        );

        assert_eq!(
            parse_expression(
                r#"
            let x = 5 ;
                y = 10 
            in x + y"#
            ),
            Ok((
                "",
                Expr::Let {
                    bound_values: vec![
                        Declaration {
                            name: "x".to_string(),
                            expr: Box::new(Expr::Literal(Literal::Integer(5)))
                        },
                        Declaration {
                            name: "y".to_string(),
                            expr: Box::new(Expr::Literal(Literal::Integer(10)))
                        },
                    ],
                    inner_expr: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOperation::Plus,
                        rhs: Box::new(Expr::Identifier("y".to_string())),
                    })
                }
            ))
        );
    }
}
