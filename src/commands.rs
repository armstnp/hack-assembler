#[derive(Debug, PartialEq, Clone)]
pub enum UnaryOp {
    Negate,
    Not,
    Identity,
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    And,
    Or,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Lhs {
    A,
    D,
    M,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Rhs {
    A,
    D,
    M,
    One,
    Zero,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Unary(UnaryOp, Rhs),
    Binary(Lhs, BinaryOp, Rhs),
}

#[derive(Debug, PartialEq, Clone, Default)]
pub struct Destination {
    pub a: bool,
    pub d: bool,
    pub m: bool,
}

#[derive(Debug, PartialEq, Clone)]
pub enum JumpCondition {
    Greater,
    Equal,
    GreaterEqual,
    Less,
    NotEqual,
    LessEqual,
    Unconditional,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Address {
    Constant(u16),
    Symbol(String),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Command {
    A(Address),
    C(Destination, Expression, Option<JumpCondition>),
    L(String),
}
