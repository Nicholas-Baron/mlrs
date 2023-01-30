use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{char, multispace0, multispace1, space0, space1},
    combinator, multi, sequence, IResult,
};

use super::{
    parse_comment, parse_identifier, parse_literal, parse_pattern, parse_separator,
    BinaryOperation, Expr,
};

pub fn parse_expression(input: &str) -> IResult<&str, Expr> {
    let (input, _) = multi::many0(branch::alt((
        parse_comment,
        parse_separator,
        combinator::value((), multispace1),
    )))(input)?;
    branch::alt((
        parse_match_expression,
        parse_let_expression,
        parse_lambda,
        parse_if_expr,
        parse_equality,
    ))(input)
}

fn parse_match_expression(input: &str) -> IResult<&str, Expr> {
    use crate::syntax::Pattern;
    fn parse_match_arms(input: &str) -> IResult<&str, Vec<(Pattern, Expr)>> {
        multi::many1(combinator::map(
            sequence::tuple((
                multispace0,
                tag("case"),
                multispace1,
                parse_pattern,
                sequence::delimited(multispace0, tag("=>"), multispace0),
                parse_expression,
            )),
            |(_, _case, _, pattern, _, result)| (pattern, result),
        ))(input)
    }

    combinator::map(
        sequence::tuple((
            tag("match"),
            multispace1,
            parse_expression,
            multispace1,
            parse_match_arms,
        )),
        |(_match, _, scrutinee, _, arms)| Expr::Match {
            scrutinee: Box::new(scrutinee),
            arms,
        },
    )(input)
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

fn parse_array(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        sequence::delimited(
            char('['),
            multi::separated_list0(char(','), parse_expression),
            char(']'),
        ),
        |elements| Expr::Array { elements },
    )(input)
}

fn parse_tuple(input: &str) -> IResult<&str, Expr> {
    combinator::map(
        combinator::verify(
            sequence::delimited(
                char('('),
                multi::separated_list1(
                    sequence::preceded(multispace0, char(',')),
                    parse_expression,
                ),
                sequence::preceded(multispace0, char(')')),
            ),
            |elements: &[_]| elements.len() >= 2,
        ),
        |elements| Expr::Tuple { elements },
    )(input)
}

fn parse_primary_expr(input: &str) -> IResult<&str, Expr> {
    let (input, _) = space0(input)?;
    branch::alt((
        parse_tuple,
        parse_array,
        sequence::delimited(char('('), parse_expression, char(')')),
        combinator::map(parse_literal, Expr::Literal),
        combinator::map(parse_identifier, Expr::Identifier),
    ))(input)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::{Literal, Pattern};

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
    fn parse_tuple_test() {
        assert!(parse_tuple("()").is_err());
        assert!(parse_tuple("(1)").is_err());
        assert_eq!(
            parse_tuple("( 1 , 2 )"),
            Ok((
                "",
                Expr::Tuple {
                    elements: vec![
                        Expr::Literal(Literal::Integer(1)),
                        Expr::Literal(Literal::Integer(2))
                    ]
                }
            ))
        );
    }

    #[test]
    fn parse_array_test() {
        assert_eq!(
            parse_array("[]"),
            IResult::Ok(("", Expr::Array { elements: vec![] }))
        );

        assert_eq!(
            parse_array("[1]"),
            IResult::Ok((
                "",
                Expr::Array {
                    elements: vec![Expr::Literal(Literal::Integer(1))]
                }
            ))
        );

        assert_eq!(
            parse_array("[1, false]"),
            IResult::Ok((
                "",
                Expr::Array {
                    elements: vec![
                        Expr::Literal(Literal::Integer(1)),
                        Expr::Literal(Literal::Boolean(false))
                    ]
                }
            ))
        );

        assert_eq!(parse_array("[1,2]"), parse_array("[1, 2]"))
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
    fn parse_match_expression_test() {
        assert_eq!(
            parse_expression("match x case _ => 5"),
            Ok((
                "",
                Expr::Match {
                    scrutinee: Box::new(Expr::Identifier("x".to_string())),
                    arms: vec![(Pattern::Ignore, Expr::Literal(Literal::Integer(5)))]
                }
            ))
        );

        assert_eq!(
            parse_expression("match x case (_, _) => 10"),
            Ok((
                "",
                Expr::Match {
                    scrutinee: Box::new(Expr::Identifier("x".to_string())),
                    arms: vec![(
                        Pattern::Tuple(vec![Pattern::Ignore, Pattern::Ignore]),
                        Expr::Literal(Literal::Integer(10))
                    )]
                }
            ))
        );

        assert_eq!(
            parse_expression("match x case (_, _) => 10 \n case _ => 5"),
            Ok((
                "",
                Expr::Match {
                    scrutinee: Box::new(Expr::Identifier("x".to_string())),
                    arms: vec![
                        (
                            Pattern::Tuple(vec![Pattern::Ignore, Pattern::Ignore]),
                            Expr::Literal(Literal::Integer(10))
                        ),
                        (Pattern::Ignore, Expr::Literal(Literal::Integer(5)))
                    ]
                }
            ))
        );
    }
}
