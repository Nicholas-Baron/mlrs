use std::fmt::{self, Display};

pub type Identifier = String;

/// This denotes a binding of some expression to a name.
/// ```
/// x = 5
/// f = \x -> \y -> x + y
/// ```
#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub name: Identifier,
    pub expr: Box<Expr>,
}

/// All expressions
#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Lambda {
        parameter: Pattern,
        body: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: BinaryOperation,
    },
    If {
        condition: Box<Expr>,
        true_value: Box<Expr>,
        false_value: Box<Expr>,
    },
    Let {
        bound_values: Vec<Declaration>,
        inner_expr: Box<Expr>,
    },
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pattern {
    Id(Identifier),
    Ignore,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinaryOperation {
    Application,
    Equality,
    Plus,
    Mult,
    Minus,
}

// TODO: Fold this into `Expr`?
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match *self {
            Literal::Boolean(b) => f.write_str(if b { "true" } else { "false" }),
            Literal::Integer(x) => f.write_fmt(format_args!("{}", x)),
        }
    }
}
