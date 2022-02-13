use super::syntax::*;

use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{self, char, multispace0, multispace1, space0, space1},
    combinator, multi, sequence, IResult,
};

fn parse_identifier(input: &str) -> IResult<&str, String> {
    use complete::{alpha1, alphanumeric0};
    combinator::verify(
        combinator::map(sequence::pair(alpha1, alphanumeric0), |(first, second)| {
            format!("{}{}", first, second)
        }),
        |id: &String| {
            let reserved = [
                "if", "then", "else", "true", "True", "false", "False", "let", "in",
            ];
            !reserved.into_iter().any(|kw| kw == id)
        },
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

fn parse_expression_separator(input: &str) -> IResult<&str, ()> {
    combinator::value(
        (),
        multi::many1_count(sequence::preceded(
            space0,
            branch::alt((complete::line_ending, tag(";"))),
        )),
    )(input)
}

pub fn parse_expression_binding_list(input: &str) -> IResult<&str, Vec<Expr>> {
    multi::separated_list1(parse_expression_separator, parse_expression_binding)(input)
}

pub fn parse_expression_binding(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multi::many0(branch::alt((parse_comment, parse_expression_separator)))(input)?;
    branch::alt((parse_binding, parse_expression))(input)
}

fn parse_binding(input: &str) -> IResult<&str, Expr> {
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
            |(_, name, _, _eq, _, expr)| Expr::Binding {
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
            |(_, name, _, args, _, _eq, _, expr)| Expr::Binding {
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

fn parse_expression(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multi::many0(branch::alt((
        parse_comment,
        parse_expression_separator,
        combinator::value((), multispace1),
    )))(input)?;
    branch::alt((
        parse_let_expression,
        parse_lambda,
        parse_if_expr,
        parse_equality,
    ))(input)
}

fn parse_let_expression(input: &str) -> IResult<&str, Expr> {
    // TODO: Permit function declarations in `let`
    let bindings = multi::separated_list0(
        multi::many1(parse_expression_separator),
        combinator::map(
            sequence::tuple((
                space0,
                parse_identifier,
                space0,
                char('='),
                space0,
                parse_expression,
            )),
            |(_, id, _, _eq, _, expr)| (id, expr),
        ),
    );

    combinator::map(
        sequence::tuple((
            tag("let"),
            multispace1,
            bindings,
            multispace1,
            tag("in"),
            space1,
            parse_expression,
        )),
        |(_let, _, bindings, _, _in, _, expr)| Expr::Let {
            bound_values: bindings.into_iter().collect(),
            inner_expr: Box::new(expr),
        },
    )(input)
}

fn parse_lambda(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        sequence::tuple((
            space0,
            char('\\'),
            space0,
            multi::separated_list1(space1, parse_identifier),
            space0,
            tag("->"),
            parse_expression,
        )),
        |(_, _slash, _, parameters, _, _arrow, body)| {
            parameters
                .into_iter()
                .rev()
                .fold(body, |body, parameter| Expr::Lambda {
                    parameter,
                    body: Box::new(body),
                })
        },
    )(input)
}

fn parse_if_expr(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        sequence::tuple((
            multispace0,
            tag("if"),
            parse_expression,
            multispace1,
            tag("then"),
            parse_expression,
            multispace1,
            tag("else"),
            parse_expression,
        )),
        |(_, _if, condition, _, _then, lhs, _, _else, rhs)| Expr::If {
            condition: Box::new(condition),
            true_value: Box::new(lhs),
            false_value: Box::new(rhs),
        },
    )(input)
}

fn parse_equality(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        multi::separated_list1(
            sequence::delimited(space0, tag("=="), space0),
            parse_addition,
        ),
        |exprs| {
            exprs
                .into_iter()
                .reduce(|lhs, rhs| Expr::Binary {
                    op: BinaryOperation::Equality,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
                .expect("separated_list1 should always give at least 1 item")
        },
    )(input)
}

fn parse_addition(input: &str) -> IResult<&str, Expr> {
    let (input, expr) = parse_multiplication(input)?;

    let (input, exprs): (_, Vec<(_, _)>) = multi::many0(sequence::tuple((
        sequence::delimited(space0, branch::alt((char('+'), char('-'))), space0),
        parse_multiplication,
    )))(input)?;

    Ok((
        input,
        exprs.into_iter().fold(expr, |lhs, (op, rhs)| Expr::Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: match op {
                '+' => BinaryOperation::Plus,
                '-' => BinaryOperation::Minus,
                _ => unreachable!(),
            },
        }),
    ))
}

fn parse_multiplication(input: &str) -> IResult<&str, Expr> {
    let (input, expr) = parse_application(input)?;

    let (input, exprs): (_, Vec<(_, _)>) = multi::many0(sequence::tuple((
        sequence::delimited(space0, branch::alt((char('*'), char('/'))), space0),
        parse_application,
    )))(input)?;

    Ok((
        input,
        exprs.into_iter().fold(expr, |lhs, (op, rhs)| Expr::Binary {
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
            op: match op {
                '*' => BinaryOperation::Mult,
                '/' => todo!("Implement division"),
                _ => unreachable!(),
            },
        }),
    ))
}

fn parse_application(input: &str) -> IResult<&str, Expr> {
    let (_, next_primary) = combinator::peek(parse_primary_expr)(input)?;

    let could_be_application = matches!(
        next_primary,
        Expr::Identifier(_)
            | Expr::Binary {
                op: BinaryOperation::Application,
                ..
            }
            | Expr::Lambda { .. }
    );

    if could_be_application {
        combinator::map(
            multi::separated_list1(space1, parse_primary_expr),
            |exprs| {
                exprs
                    .into_iter()
                    .reduce(|lhs, rhs| Expr::Binary {
                        op: BinaryOperation::Application,
                        lhs: Box::new(lhs),
                        rhs: Box::new(rhs),
                    })
                    .expect("separated_list1 should always give at least 1 item")
            },
        )(input)
    } else {
        parse_primary_expr(input)
    }
}

fn parse_primary_expr(input: &str) -> IResult<&str, Expr> {
    use complete::digit1;
    let (input, _) = space0(input)?;
    branch::alt((
        sequence::delimited(char('('), parse_expression, char(')')),
        combinator::map_res(digit1, |value: &str| {
            value.parse().map(|x| Expr::Literal(Literal::Integer(x)))
        }),
        combinator::map(parse_boolean_literal, |x| {
            Expr::Literal(Literal::Boolean(x))
        }),
        combinator::map(parse_identifier, Expr::Identifier),
    ))(input)
}

fn parse_boolean_literal(input: &str) -> IResult<&str, bool> {
    combinator::map_res(
        branch::alt((tag("true"), tag("True"), tag("false"), tag("False"))),
        |value: &str| value.to_lowercase().parse(),
    )(input)
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
    fn parse_primary_expression_test() {
        assert_eq!(
            parse_expression("x"),
            Ok(("", Expr::Identifier("x".to_string())))
        );
        assert_eq!(
            parse_expression("123"),
            Ok(("", Expr::Literal(Literal::Integer(123))))
        );
        assert_eq!(
            parse_expression("true"),
            Ok(("", Expr::Literal(Literal::Boolean(true))))
        );
        assert_eq!(
            parse_expression("True"),
            Ok(("", Expr::Literal(Literal::Boolean(true))))
        );
        assert_eq!(
            parse_expression("false"),
            Ok(("", Expr::Literal(Literal::Boolean(false))))
        );
        assert_eq!(
            parse_expression("False"),
            Ok(("", Expr::Literal(Literal::Boolean(false))))
        );
    }

    #[test]
    fn parse_application_test() {
        assert_eq!(
            parse_expression("f x"),
            Ok((
                "",
                Expr::Binary {
                    lhs: Box::new(Expr::Identifier("f".to_string())),
                    rhs: Box::new(Expr::Identifier("x".to_string())),
                    op: BinaryOperation::Application
                }
            ))
        );
        assert_eq!(
            parse_expression("f x y"),
            Ok((
                "",
                Expr::Binary {
                    lhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("f".to_string())),
                        rhs: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOperation::Application
                    }),
                    op: BinaryOperation::Application,
                    rhs: Box::new(Expr::Identifier("y".to_string()))
                }
            ))
        );
        assert_eq!(
            parse_expression("(\\ x -> x) y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Application,
                    lhs: Box::new(Expr::Lambda {
                        parameter: "x".to_string(),
                        body: Box::new(Expr::Identifier("x".to_string()))
                    }),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
        assert!(matches!(parse_expression("5 x"), Ok((" x", _))));
    }

    #[test]
    fn parse_lambda_test() {
        assert_eq!(
            parse_expression("\\ x -> x"),
            Ok((
                "",
                Expr::Lambda {
                    parameter: "x".to_string(),
                    body: Box::new(Expr::Identifier("x".to_string()))
                }
            ))
        );
        assert_eq!(
            parse_expression("\\ x -> \\ y -> x"),
            Ok((
                "",
                Expr::Lambda {
                    parameter: "x".to_string(),
                    body: Box::new(Expr::Lambda {
                        parameter: "y".to_string(),
                        body: Box::new(Expr::Identifier("x".to_string()))
                    })
                }
            ))
        );
    }

    #[test]
    fn parse_addition_test() {
        assert_eq!(
            parse_expression("x + y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Plus,
                    lhs: Box::new(Expr::Identifier("x".to_string())),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("x + y + z"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Plus,
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOperation::Plus,
                        lhs: Box::new(Expr::Identifier("x".to_string())),
                        rhs: Box::new(Expr::Identifier("y".to_string())),
                    }),
                    rhs: Box::new(Expr::Identifier("z".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("f x + y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Plus,
                    lhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("f".to_string())),
                        rhs: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOperation::Application,
                    }),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
    }

    #[test]
    fn parse_subtraction_test() {
        assert_eq!(
            parse_expression("x - y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Minus,
                    lhs: Box::new(Expr::Identifier("x".to_string())),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("x + y - z"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Minus,
                    lhs: Box::new(Expr::Binary {
                        op: BinaryOperation::Plus,
                        lhs: Box::new(Expr::Identifier("x".to_string())),
                        rhs: Box::new(Expr::Identifier("y".to_string())),
                    }),
                    rhs: Box::new(Expr::Identifier("z".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("f x - y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Minus,
                    lhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("f".to_string())),
                        rhs: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOperation::Application,
                    }),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
    }

    #[test]
    fn parse_multiplication_test() {
        assert_eq!(
            parse_expression("x * y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Mult,
                    lhs: Box::new(Expr::Identifier("x".to_string())),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("x + y * z"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Plus,
                    lhs: Box::new(Expr::Identifier("x".to_string())),
                    rhs: Box::new(Expr::Binary {
                        op: BinaryOperation::Mult,
                        lhs: Box::new(Expr::Identifier("y".to_string())),
                        rhs: Box::new(Expr::Identifier("z".to_string())),
                    }),
                }
            ))
        );
        assert_eq!(
            parse_expression("f x * y"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Mult,
                    lhs: Box::new(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("f".to_string())),
                        rhs: Box::new(Expr::Identifier("x".to_string())),
                        op: BinaryOperation::Application,
                    }),
                    rhs: Box::new(Expr::Identifier("y".to_string())),
                }
            ))
        );
    }

    #[test]
    fn parse_if_expression_test() {
        assert_eq!(
            parse_expression("if x then y else z"),
            Ok((
                "",
                Expr::If {
                    condition: Box::new(Expr::Identifier("x".to_string())),
                    true_value: Box::new(Expr::Identifier("y".to_string())),
                    false_value: Box::new(Expr::Identifier("z".to_string())),
                }
            ))
        );
        assert_eq!(
            parse_expression("if x then y else z + 1"),
            Ok((
                "",
                Expr::If {
                    condition: Box::new(Expr::Identifier("x".to_string())),
                    true_value: Box::new(Expr::Identifier("y".to_string())),
                    false_value: Box::new(Expr::Binary {
                        op: BinaryOperation::Plus,
                        lhs: Box::new(Expr::Identifier("z".to_string())),
                        rhs: Box::new(Expr::Literal(Literal::Integer(1)))
                    })
                }
            ))
        );
        assert_eq!(
            parse_expression("if x then y else if z then a else b"),
            Ok((
                "",
                Expr::If {
                    condition: Box::new(Expr::Identifier("x".to_string())),
                    true_value: Box::new(Expr::Identifier("y".to_string())),
                    false_value: Box::new(Expr::If {
                        condition: Box::new(Expr::Identifier("z".to_string())),
                        true_value: Box::new(Expr::Identifier("a".to_string())),
                        false_value: Box::new(Expr::Identifier("b".to_string())),
                    }),
                }
            ))
        );
    }

    #[test]
    fn parse_binding_test() {
        assert_eq!(
            parse_expression_binding("x = 5"),
            Ok((
                "",
                Expr::Binding {
                    name: "x".to_string(),
                    expr: Box::new(Expr::Literal(Literal::Integer(5)))
                }
            ))
        );

        assert_eq!(
            parse_expression_binding("x = (\\ a b -> a + b) 5"),
            Ok((
                "",
                Expr::Binding {
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
        let (rest, exprs) =
            parse_expression_binding_list("f x y = x + y\n\n double x = x * 2").unwrap();
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
            parse_expression_binding("f x y = x + y"),
            parse_expression_binding("f = \\x -> \\y -> x+y")
        );
    }

    #[test]
    fn parse_extra_newlines_test() {
        assert_eq!(
            parse_expression_binding("f x y = x + y"),
            parse_expression_binding("f x y\n    = x + y")
        );
        let (rest, expr) = parse_expression_binding(
            r"
fib x = if x == 0 then 0
   else if x == 1 then 1
   else fib (x - 1)
                ",
        )
        .unwrap();
        assert_eq!(rest.trim(), "");
        assert!(matches!(expr, Expr::Binding { .. }));
    }

    #[test]
    fn parse_equality_test() {
        assert_eq!(
            parse_expression("x == 0"),
            Ok((
                "",
                Expr::Binary {
                    op: BinaryOperation::Equality,
                    lhs: Box::new(Expr::Identifier("x".to_string())),
                    rhs: Box::new(Expr::Literal(Literal::Integer(0)))
                }
            ))
        );
    }

    #[test]
    fn parse_let_test() {
        use std::collections::HashMap;
        let mut bound_values = HashMap::new();
        bound_values.insert("x".to_string(), Expr::Literal(Literal::Integer(5)));

        assert_eq!(
            parse_expression("let x = 5 in x"),
            Ok((
                "",
                Expr::Let {
                    bound_values,
                    inner_expr: Box::new(Expr::Identifier("x".to_string()))
                }
            ))
        );

        let mut bound_values = HashMap::new();
        bound_values.insert("x".to_string(), Expr::Literal(Literal::Integer(5)));
        bound_values.insert("y".to_string(), Expr::Literal(Literal::Integer(10)));

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
                    bound_values,
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
