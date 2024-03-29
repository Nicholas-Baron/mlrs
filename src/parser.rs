use super::syntax::*;

use nom::{
    branch,
    bytes::complete::tag,
    character::complete::{self, char, multispace0, multispace1, space0, space1},
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

// Taken from the nom examples @ https://github.com/rust-bakery/nom/blob/main/examples/string.rs
fn parse_escaped_string(input: &str) -> IResult<&str, String> {
    enum Fragment<'a> {
        Literal(&'a str),
        EscapedChar(char),
        EscapedWS,
    }

    fn parse_literal(input: &str) -> IResult<&str, &str> {
        let not_quote_slash = nom::bytes::complete::is_not("\"\\");

        combinator::verify(not_quote_slash, |s: &str| !s.is_empty())(input)
    }

    let escapable = branch::alt((char('n'), char('t'), char('0')));

    let build_string = multi::fold_many0(
        branch::alt((
            combinator::map(parse_literal, Fragment::Literal),
            combinator::map(sequence::preceded(char('\\'), escapable), Fragment::EscapedChar),
            combinator::map(sequence::preceded(char('\\'), multispace1), |_| {
                Fragment::EscapedWS
            }),
        )),
        String::new,
        |mut acc, fragment: Fragment| {
            match fragment {
                Fragment::Literal(lit) => acc.push_str(lit),
                Fragment::EscapedChar(_) => todo!(),
                Fragment::EscapedWS => {}
            }
            acc
        },
    );

    sequence::delimited(char('"'), build_string, char('"'))(input)
}

fn parse_literal(input: &str) -> IResult<&str, Literal> {
    use complete::digit1;
    branch::alt((
        combinator::map_res(digit1, |value: &str| value.parse().map(Literal::Integer)),
        combinator::map(parse_boolean_literal, Literal::Boolean),
        combinator::map(parse_escaped_string, Literal::String),
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

    let parse_list_cons = combinator::verify(
        sequence::delimited(
            char('('),
            multi::separated_list1(sequence::preceded(multispace0, char(':')), parse_pattern),
            char(')'),
        ),
        |elements: &[_]| elements.len() >= 2,
    );

    branch::alt((
        combinator::map(parse_literal, Pattern::Literal),
        combinator::map(parse_identifier, Pattern::Id),
        combinator::map(char('_'), |_| Pattern::Ignore),
        combinator::map(parse_tuple, Pattern::Tuple),
        combinator::map(parse_list_cons, Pattern::ListCons),
        combinator::map(tag("[]"), |_| Pattern::EmptyList),
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

fn parse_function_declaration(input: &str) -> IResult<&str, Declaration> {
    let (input, name) = sequence::preceded(multispace0, parse_identifier)(input)?;

    fn parse_function_clause(input: &str) -> IResult<&str, (Vec<Pattern>, Expr)> {
        // There may be some parameters to parse
        // TODO: Allow parameters across multiple lines
        //       Currently, expressions on preceding lines look like more parameters.
        //       A keyword could help denote the separation
        let (input, params) =
            sequence::preceded(space1, multi::separated_list1(space1, parse_pattern))(input)?;

        let (input, body) = sequence::preceded(
            sequence::delimited(multispace0, char('='), multispace0),
            parse_expression,
        )(input)?;

        Ok((input, (params, body)))
    }

    let (input, clauses) = multi::separated_list1(
        sequence::preceded(multispace0, char('|')),
        parse_function_clause,
    )(input)?;

    Ok((input, Declaration::Function { name, clauses }))
}

pub fn parse_declaration(input: &str) -> IResult<&str, Declaration> {
    let parse_pattern_declaration = |input| {
        let (input, pattern) = parse_pattern(input)?;

        let (input, _) = sequence::delimited(multispace0, char('='), multispace0)(input)?;

        let (input, body) = parse_expression(input)?;

        Ok((
            input,
            Declaration::PatternBinding {
                pattern,
                expr: body,
            },
        ))
    };

    branch::alt((parse_function_declaration, parse_pattern_declaration))(input)
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
                    DeclOrExpr::Decl(Declaration::Function {
                        name: "func".to_string(),
                        clauses: vec![(
                            vec![Pattern::Id("x".to_string())],
                            Expr::Binary {
                                lhs: Box::new(Expr::Identifier("x".to_string())),
                                rhs: Box::new(Expr::Literal(Literal::Integer(2))),
                                op: BinaryOperation::Mult
                            }
                        )]
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
                    DeclOrExpr::Decl(Declaration::Function {
                        name: "func".to_string(),
                        clauses: vec![(
                            vec![Pattern::Id("x".to_string())],
                            Expr::Match {
                                scrutinee: Box::new(Expr::Identifier("x".to_string())),
                                arms: vec![(Pattern::Ignore, Expr::Literal(Literal::Integer(10)))]
                            }
                        )]
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

        assert_eq!(
            parse_whole_file(
                r#"
                x true
                const x = 10
                "#
            ),
            Ok((
                "",
                vec![
                    DeclOrExpr::Expr(Expr::Binary {
                        lhs: Box::new(Expr::Identifier("x".to_string())),
                        rhs: Box::new(Expr::Literal(Literal::Boolean(true))),
                        op: BinaryOperation::Application,
                    }),
                    DeclOrExpr::Decl(Declaration::Function {
                        name: "const".to_string(),
                        clauses: vec![(
                            vec![Pattern::Id("x".to_string())],
                            Expr::Literal(Literal::Integer(10)),
                        )]
                    })
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
                Declaration::simple_name("x".to_string(), Expr::Literal(Literal::Integer(5)))
            ))
        );

        assert_eq!(
            parse_declaration("(x,y) = (1,2)"),
            Ok((
                "",
                Declaration::PatternBinding {
                    pattern: Pattern::Tuple(vec![
                        Pattern::Id("x".to_string()),
                        Pattern::Id("y".to_string())
                    ]),
                    expr: Expr::Tuple {
                        elements: vec![
                            Expr::Literal(Literal::Integer(1)),
                            Expr::Literal(Literal::Integer(2))
                        ]
                    }
                }
            ))
        );

        assert_eq!(
            parse_declaration("x = (\\ a b -> a + b) 5"),
            Ok((
                "",
                Declaration::simple_name(
                    "x".to_string(),
                    Expr::Binary {
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
                    }
                )
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
        assert!(matches!(decl, Declaration::Function { .. }));
    }

    #[test]
    fn parse_let_test() {
        assert_eq!(
            parse_expression("let x = 5 in x"),
            Ok((
                "",
                Expr::Let {
                    bound_values: vec![Declaration::simple_name(
                        "x".to_string(),
                        Expr::Literal(Literal::Integer(5))
                    )],
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
                        Declaration::simple_name(
                            "x".to_string(),
                            Expr::Literal(Literal::Integer(5))
                        ),
                        Declaration::simple_name(
                            "y".to_string(),
                            Expr::Literal(Literal::Integer(10))
                        ),
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
                Declaration::Function {
                    name: "const".to_string(),
                    clauses: vec![(
                        vec![Pattern::Id("x".to_string()), Pattern::Ignore],
                        Expr::Identifier("x".to_string())
                    )]
                }
            ))
        );
    }

    #[test]
    fn parse_function_declaration_test() {
        assert_eq!(
            parse_declaration(
                r#"map _ [] = [] 
                     | f (x:xs) = f x : map f xs"#
            ),
            Ok((
                "",
                Declaration::Function {
                    name: "map".to_string(),
                    clauses: vec![
                        (
                            vec![Pattern::Ignore, Pattern::EmptyList],
                            Expr::List { elements: vec![] }
                        ),
                        (
                            vec![
                                Pattern::Id("f".to_string()),
                                Pattern::ListCons(vec![
                                    Pattern::Id("x".to_string()),
                                    Pattern::Id("xs".to_string())
                                ])
                            ],
                            Expr::Binary {
                                lhs: Box::new(Expr::Binary {
                                    lhs: Box::new(Expr::Identifier("f".to_string())),
                                    rhs: Box::new(Expr::Identifier("x".to_string())),
                                    op: BinaryOperation::Application
                                }),
                                rhs: Box::new(Expr::Binary {
                                    lhs: Box::new(Expr::Binary {
                                        lhs: Box::new(Expr::Identifier("map".to_string())),
                                        rhs: Box::new(Expr::Identifier("f".to_string())),
                                        op: BinaryOperation::Application
                                    }),
                                    rhs: Box::new(Expr::Identifier("xs".to_string())),
                                    op: BinaryOperation::Application
                                }),
                                op: BinaryOperation::Prepend
                            }
                        )
                    ]
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
