use super::literals::{Literal, literal};
pub use super::misc::{identifier_name};


#[derive(Debug, PartialEq)]
pub enum Expression {
    ThisExpression,
    Literal(Literal),
    Assignment {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>
    },
    Binary {
        left: Box<Expression>,
        operator: String,
        right: Box<Expression>
    },
    Identifier {
        name: String
    },
}

named!(literal_expression< &str, Expression >, map!(
    literal,
    |literal| (Expression::Literal(literal))
));

named!(this_expression< &str, Expression >, do_parse!(
    tag!("this") >>
    (Expression::ThisExpression)
));

named!(assignment_operators< &str, &str >, ws!(alt_complete!(
    // order: longest to the shortest
    // 4 chars
    tag!(">>>=") |
    // 3 chars
    tag!("<<=") |
    tag!(">>=") |
    // 2 chars
    tag!("+=") |
    tag!("-=") |
    tag!("*=") |
    tag!("/=") |
    tag!("%=") |
    tag!("|=") |
    tag!("^=") |
    tag!("&=") |
    // 1 char
    tag!("=")
)));

named!(assignment_expression< &str, Expression >, do_parse!(
    left: identifier_expression >>
    operator: assignment_operators >>
    right: expression >>
    (Expression::Assignment {
        left: Box::new(left),
        operator: operator.to_string(),
        right: Box::new(right)
    })
));

named!(binary_operators< &str, &str >, ws!(alt_complete!(
    // order: longest to the shortest
    tag!("instanceof") |
    // 3 chars
    tag!("===") |
    tag!("!==") |
    tag!(">>>") |
    // 2 chars
    tag!("in") |
    tag!("==") |
    tag!("!=") |
    tag!("<=") |
    tag!(">=") |
    tag!("<<") |
    tag!(">>") |
    // 1 char
    tag!("<") |
    tag!(">") |
    tag!("+") |
    tag!("-") |
    tag!("*") |
    tag!("/") |
    tag!("%") |
    tag!("|") |
    tag!("^") |
    tag!("&")
)));

named!(binary_expression< &str, Expression >, do_parse!(
    left: identifier_expression >>
    operator: binary_operators >>
    right: expression >>
    (Expression::Binary {
        left: Box::new(left),
        operator: operator.to_string(),
        right: Box::new(right)
    })
));


named!(identifier_expression < &str, Expression >, do_parse!(
    id: identifier_name >>
    (Expression::Identifier {
        name: id.to_string()
    })
));

named!(pub expression< &str, Expression >, ws!(alt_complete!(
    this_expression |
    literal_expression |
    assignment_expression |
    binary_expression |
    identifier_expression
)));

