use super::syntax::*;

use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{self, char, space0, space1},
    combinator, multi, sequence, IResult,
};

fn parse_identifier(input: &str) -> IResult<&str, String> {
    use complete::{alpha1, alphanumeric0};
    combinator::map(sequence::pair(alpha1, alphanumeric0), |(first, second)| {
        format!("{}{}", first, second)
    })(input)
}

pub fn parse_expression(input: &str) -> IResult<&str, Expr> {
    branch::alt((parse_binding, parse_lambda))(input)
}

fn parse_binding(input: &str) -> IResult<&str, Expr> {
    branch::alt((
        combinator::map(
            sequence::tuple((
                space0,
                parse_identifier,
                space0,
                char('='),
                space0,
                parse_lambda,
            )),
            |(_, name, _, _eq, _, expr)| Expr::Binding {
                name,
                expr: Box::new(expr),
            },
        ),
        combinator::map(
            sequence::tuple((
                space0,
                parse_identifier,
                space1,
                multi::separated_list1(space1, parse_identifier),
                space0,
                char('='),
                space0,
                parse_lambda,
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

fn parse_lambda(input: &str) -> IResult<&str, Expr> {
    branch::alt((
        parse_addition,
        combinator::map(
            sequence::tuple((
                space0,
                char('\\'),
                space0,
                parse_identifier,
                space0,
                tag("->"),
                parse_expression,
            )),
            |(_, _slash, _, parameter, _, _arrow, body)| Expr::Lambda {
                parameter,
                body: Box::new(body),
            },
        ),
    ))(input)
}

fn parse_addition(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        multi::separated_list1(
            sequence::delimited(space0, char('+'), space0),
            parse_application,
        ),
        |exprs| {
            exprs
                .into_iter()
                .reduce(|lhs, rhs| Expr::Binary {
                    op: BinaryOperation::Plus,
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                })
                .expect("separated_list1 should always give at least 1 item")
        },
    )(input)
}

fn parse_application(input: &str) -> IResult<&str, Expr> {
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
    fn parse_binding_test() {
        assert_eq!(
            parse_expression("x = 5"),
            Ok((
                "",
                Expr::Binding {
                    name: "x".to_string(),
                    expr: Box::new(Expr::Literal(Literal::Integer(5)))
                }
            ))
        );
    }

    #[test]
    fn parse_function_binding_test() {
        assert_eq!(
            parse_expression("f x y = x + y"),
            parse_expression("f = \\x -> \\y -> x+y")
        );
    }
}
