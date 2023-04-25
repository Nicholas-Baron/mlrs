use std::fmt::{self, Display};

pub type Identifier = String;

/// This denotes a binding of some expression to a name.
/// ```
/// x = 5
/// f = \x -> \y -> x + y
/// ```
#[derive(Debug, PartialEq)]
pub struct Declaration {
    pub pattern: Pattern,
    pub expr: Expr,
}

impl Declaration {
    pub fn simple_name(id: Identifier, expr: Expr) -> Self {
        Self {
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
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pattern {
    Id(Identifier),
    Literal(Literal),
    Tuple(Vec<Pattern>),
    ListCons(Vec<Pattern>),
    Ignore,
}

impl Pattern {
    pub fn bound_names(&self) -> Box<dyn Iterator<Item = &Identifier> + '_> {
        use std::iter;
        match self {
            Pattern::Id(id) => Box::new(iter::once(id)),
            Pattern::Tuple(subpatterns) | Pattern::ListCons(subpatterns) => Box::new(
                subpatterns
                    .iter()
                    .flat_map(|subpattern| subpattern.bound_names()),
            ),
            Pattern::Literal(_) | Pattern::Ignore => Box::new(iter::empty()),
        }
    }
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
