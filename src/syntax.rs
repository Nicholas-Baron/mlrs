use std::fmt::{self, Display};

pub type Identifier = String;

/// This denotes a binding of some expression to a name.
/// ```
/// x = 5
/// f 0 _ = 0
/// | x y = x + y
/// ```
#[derive(Debug, PartialEq)]
pub enum Declaration {
    PatternBinding {
        pattern: Pattern,
        expr: Expr,
    },
    Function {
        name: Identifier,
        clauses: Vec<(Vec<Pattern>, Expr)>,
    },
}

#[cfg(test)]
impl Declaration {
    pub fn simple_name(id: Identifier, expr: Expr) -> Self {
        Self::PatternBinding {
            pattern: Pattern::Id(id),
            expr,
        }
    }
}

/// All expressions
#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Tuple {
        elements: Vec<Expr>,
    },
    List {
        elements: Vec<Expr>,
    },
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
    Match {
        scrutinee: Box<Expr>,
        arms: Vec<(Pattern, Expr)>,
    },
    Dictionary(Vec<(Expr, Expr)>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Id(Identifier),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    ListCons(Vec<Pattern>),
    EmptyList,
    Ignore,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinaryOperation {
    Application,
    Equality,
    Plus,
    Mult,
    Minus,
    Prepend,
    Concat,
    Modulo,
    Division,
}

// TODO: Fold this into `Expr`?
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    String(String),
}

impl Display for Literal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            Literal::Boolean(b) => f.write_str(if *b { "true" } else { "false" }),
            Literal::Integer(x) => f.write_fmt(format_args!("{}", x)),
            Literal::String(s) => f.write_str(s),
        }
    }
}
