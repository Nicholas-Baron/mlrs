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

#[derive(PartialEq, Debug)]
pub enum BinaryOperation {
    Application,
}

#[derive(PartialEq, Debug)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
}
