use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{self, char, multispace0, multispace1, space0, space1},
    combinator, multi, sequence, IResult,
};

use super::{
    parse_comment, parse_identifier, parse_pattern, parse_separator, BinaryOperation, Expr, Literal,
};

pub fn parse_expression(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multi::many0(branch::alt((
        parse_comment,
        parse_separator,
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
    combinator::map(
        sequence::tuple((
            tag("let"),
            multispace1,
            super::parse_declaration_list,
            multispace1,
            tag("in"),
            space1,
            parse_expression,
        )),
        |(_let, _, bindings, _, _in, _, expr)| Expr::Let {
            bound_values: bindings,
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
            multi::separated_list1(space1, parse_pattern),
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
    use crate::syntax::Pattern;

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
                        parameter: Pattern::Id("x".to_string()),
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
                    parameter: Pattern::Id("x".to_string()),
                    body: Box::new(Expr::Identifier("x".to_string()))
                }
            ))
        );
        assert_eq!(
            parse_expression("\\ x -> \\ y -> x"),
            Ok((
                "",
                Expr::Lambda {
                    parameter: Pattern::Id("x".to_string()),
                    body: Box::new(Expr::Lambda {
                        parameter: Pattern::Id("y".to_string()),
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
}
