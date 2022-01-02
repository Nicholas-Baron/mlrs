use std::fmt::{self, Display};

pub type Identifier = String;

/// All expressions
#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    /// This denotes a binding of some expression to a name.
    /// ```
    /// x = 5
    /// f = \x -> \y -> x + y
    /// ```
    Binding {
        name: Identifier,
        expr: Box<Expr>,
    },
    Lambda {
        parameter: Identifier,
        body: Box<Expr>,
    },
    Binary {
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        op: BinaryOperation,
    },
}

#[derive(PartialEq, Debug, Clone)]
pub enum BinaryOperation {
    Application,
    Plus,
}

// TODO: Fold this into `Expr`?
#[derive(PartialEq, Debug, Clone)]
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
