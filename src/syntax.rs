pub type Identifier = String;

/// All expressions
#[derive(PartialEq, Debug)]
pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
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
