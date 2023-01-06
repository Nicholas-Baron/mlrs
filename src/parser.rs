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
    "if", "then", "else", "true", "True", "false", "False", "let", "in", "match", "case",
];

fn parse_boolean_literal(input: &str) -> IResult<&str, bool> {
    combinator::map_res(
        branch::alt((tag("true"), tag("True"), tag("false"), tag("False"))),
        |value: &str| value.to_lowercase().parse(),
    )(input)
}

fn parse_literal(input: &str) -> IResult<&str, Literal> {
    use complete::digit1;
    branch::alt((
        combinator::map_res(digit1, |value: &str| value.parse().map(Literal::Integer)),
        combinator::map(parse_boolean_literal, Literal::Boolean),
    ))(input)
}

fn parse_pattern(input: &str) -> IResult<&str, Pattern> {
    let (input, _) = multispace0(input)?;

    let parse_tuple = combinator::verify(
        sequence::delimited(
            char('('),
            multi::separated_list1(sequence::preceded(multispace0, char(',')), parse_pattern),
            sequence::preceded(multispace0, char(')')),
        ),
        |elements: &[_]| elements.len() >= 2,
    );

    branch::alt((
        combinator::map(parse_literal, Pattern::Literal),
        combinator::map(parse_identifier, Pattern::Id),
        combinator::map(char('_'), |_| Pattern::Ignore),
        combinator::map(parse_tuple, Pattern::Tuple),
    ))(input)
}

fn parse_identifier(input: &str) -> IResult<&str, String> {
    use complete::{alpha1, alphanumeric0};
    combinator::verify(
        combinator::map(sequence::pair(alpha1, alphanumeric0), |(first, second)| {
            format!("{}{}", first, second)
        }),
        |id: &String| !RESERVED_WORDS.iter().any(|kw| kw == id),
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

#[derive(Debug, PartialEq)]
pub enum DeclOrExpr {
    Decl(Declaration),
    Expr(Expr),
}

pub fn parse_whole_file(input: &str) -> IResult<&str, Vec<DeclOrExpr>> {
    combinator::all_consuming(sequence::terminated(parse_decl_or_expr_list, multispace0))(input)
}

pub fn parse_decl_or_expr(input: &str) -> IResult<&str, DeclOrExpr> {
    branch::alt((
        combinator::map(parse_declaration, DeclOrExpr::Decl),
        combinator::map(parse_expression, DeclOrExpr::Expr),
    ))(input)
}

pub fn parse_decl_or_expr_list(input: &str) -> IResult<&str, Vec<DeclOrExpr>> {
    multi::separated_list0(multi::many1(parse_separator), parse_decl_or_expr)(input)
}

pub fn parse_declaration_list(input: &str) -> IResult<&str, Vec<Declaration>> {
    multi::separated_list0(multi::many1(parse_separator), parse_declaration)(input)
}

pub fn parse_declaration(input: &str) -> IResult<&str, Declaration> {
    let (input, name) = sequence::preceded(multispace0, parse_identifier)(input)?;

    // There may be some parameters to parse
    let (input, params) = sequence::preceded(
        multispace1,
        multi::separated_list1(multispace1, parse_pattern),
    )(input)
    .unwrap_or((input, vec![]));

    let (input, body) = sequence::preceded(
        sequence::delimited(multispace0, char('='), multispace0),
        parse_expression,
    )(input)?;

    Ok((
        input,
        Declaration {
            name,
            expr: params
                .into_iter()
                .rev()
                .fold(Box::new(body), |body, parameter| {
                    Box::new(Expr::Lambda { body, parameter })
                }),
        },
    ))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_whole_file_test() {
        assert_eq!(
            parse_whole_file(
                r#"
(10, 0)

func x = x * 2
            "#,
            ),
            Ok((
                "",
                vec![
                    DeclOrExpr::Expr(Expr::Tuple {
                        elements: vec![
                            Expr::Literal(Literal::Integer(10)),
                            Expr::Literal(Literal::Integer(0))
                        ]
                    }),
                    DeclOrExpr::Decl(Declaration {
                        name: "func".to_string(),
                        expr: Box::new(Expr::Lambda {
                            parameter: Pattern::Id("x".to_string()),
                            body: Box::new(Expr::Binary {
                                lhs: Box::new(Expr::Identifier("x".to_string())),
                                rhs: Box::new(Expr::Literal(Literal::Integer(2))),
                                op: BinaryOperation::Mult
                            })
                        })
                    })
                ]
            ))
        );

        assert_eq!(
            parse_whole_file(
                r#"
(10, 0)

func x = match x
    case _ => 10

(9, 1)
            "#,
            ),
            Ok((
                "",
                vec![
                    DeclOrExpr::Expr(Expr::Tuple {
                        elements: vec![
                            Expr::Literal(Literal::Integer(10)),
                            Expr::Literal(Literal::Integer(0))
                        ]
                    }),
                    DeclOrExpr::Decl(Declaration {
                        name: "func".to_string(),
                        expr: Box::new(Expr::Lambda {
                            parameter: Pattern::Id("x".to_string()),
                            body: Box::new(Expr::Match {
                                scrutinee: Box::new(Expr::Identifier("x".to_string())),
                                arms: vec![(Pattern::Ignore, Expr::Literal(Literal::Integer(10)))]
                            })
                        })
                    }),
                    DeclOrExpr::Expr(Expr::Tuple {
                        elements: vec![
                            Expr::Literal(Literal::Integer(9)),
                            Expr::Literal(Literal::Integer(1))
                        ]
                    }),
                ]
            ))
        );
    }

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
                            parameter: Pattern::Id("a".to_string()),
                            body: Box::new(Expr::Lambda {
                                parameter: Pattern::Id("b".to_string()),
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

    #[test]
    fn parse_ignore_test() {
        assert_eq!(
            parse_declaration("const x _ = x"),
            Ok((
                "",
                Declaration {
                    name: "const".to_string(),
                    expr: Box::new(Expr::Lambda {
                        parameter: Pattern::Id("x".to_string()),
                        body: Box::new(Expr::Lambda {
                            parameter: Pattern::Ignore,
                            body: Box::new(Expr::Identifier("x".to_string()))
                        })
                    })
                }
            ))
        );
    }

    #[test]
    fn parse_pattern_test() {
        assert_eq!(parse_pattern("_"), Ok(("", Pattern::Ignore)));
        assert_eq!(
            parse_pattern("0"),
            Ok(("", Pattern::Literal(Literal::Integer(0))))
        );
        assert_eq!(parse_pattern("x"), Ok(("", Pattern::Id("x".to_string()))));
        assert_eq!(
            parse_pattern("(_, _)"),
            Ok(("", Pattern::Tuple(vec![Pattern::Ignore, Pattern::Ignore])))
        );
    }
}
