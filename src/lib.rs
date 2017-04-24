#[macro_use]
extern crate nom;

#[macro_use] pub mod errors;
#[cfg(test)] mod tests;
mod literals;
pub use self::literals::{Literal, literal};
use nom::{ErrorKind};

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

#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: String,
    pub init: Option<Expression>
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: String
}

#[derive(Debug, PartialEq)]
pub enum ForInitializer {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    // FIXME either there is something smarted to do, or we should go for generalize it
    // everywhere
    VariableDeclaration(VariableDeclaration),
    If {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>
    },
    For {
        init: Option<ForInitializer>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Box<Statement>
    },
    Block {
        body: Vec<Statement>
    },
    Empty,
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>
}

fn var_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

named!(identifier_name< &str, &str >, do_parse!(
    return_error!(es_error!(INVALID_VAR_NAME), peek!(none_of!("0123456789"))) >>
    id: take_while1_s!(var_name_char) >>
    (id)
));


named!(variable_declarator< &str, VariableDeclarator >, do_parse!(
    id: identifier_name >>
    init: opt!(
        do_parse!(
            ws!(tag!("=")) >>
            res: expression >>
            (res)
        )
    ) >>
    (VariableDeclarator {
        id: id.to_string(),
        init: init
    })
));

named!(for_statement< &str, Statement >, do_parse!(
    ws!(tag!("for")) >>
    ws!(tag!("(")) >>
    init: alt_complete!(
        variable_declaration    => { |var_decl| (Some(ForInitializer::VariableDeclaration(var_decl))) } |
        expression              => { |expr| (Some(ForInitializer::Expression(expr))) } |
        tag!("")                => { |_| (None) }
    ) >>
    ws!(tag!(";")) >>
    test: opt!(expression) >>
    ws!(tag!(";")) >>
    update: opt!(expression) >>
    ws!(tag!(")")) >>
    body: statement >>
    (Statement::For {
        init: init,
        test: test,
        update: update,
        body: Box::new(body)
    })
));

named!(if_statement< &str, Statement >, do_parse!(
    ws!(tag!("if")) >>
    test: delimited!(tag!("("), expression, tag!(")")) >>
    consequent: call!(block_statement) >>
    // opt!(complete!(…)) so it doesn't return an incomplete state
    // FIXME: isn't there a smarter way to handle this?
    alternate_opt: opt!(complete!(preceded!(ws!(tag!("else")), block_statement))) >>
    (Statement::If {
        test: test,
        consequent: Box::new(consequent),
        alternate: match alternate_opt {
            Some(alternate) => Some(Box::new(alternate)),
            None => None
        }
    })
));

named!(empty_statement< &str, Statement >, do_parse!(
    tag!(";") >>
    (Statement::Empty)
));

named!(block_statement< &str, Statement >, map!(
    ws!(delimited!(
        tag!("{"),
        call!(body),
        tag!("}")
    )), |body| (Statement::Block { body: body })
));


named!(variable_declaration< &str, VariableDeclaration >, do_parse!(
    var_type: tag!("var") >>
    take_while1_s!(char::is_whitespace) >>
    declarators: ws!(separated_list!(tag!(","), variable_declarator)) >>
    (VariableDeclaration {
        declarations: declarators,
        kind: var_type.to_string()
    })
));

named!(variable_declaration_statement< &str, Statement >, map!(
    variable_declaration,
    |var_decl| (Statement::VariableDeclaration(var_decl))
));

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

named!(expression< &str, Expression >, ws!(alt_complete!(
    this_expression |
    literal_expression |
    assignment_expression |
    binary_expression |
    identifier_expression
)));

named!(expression_statement< &str, Statement >, do_parse!(
    expression: call!(expression) >>
    (Statement::Expression(expression))
));

named!(statement< &str, Statement >, alt_complete!(
    empty_statement |
    terminated!(variable_declaration_statement, tag!(";")) |
    terminated!(expression_statement, tag!(";")) |
    block_statement |
    if_statement |
    for_statement
));

// TODO support ASI
named!(body< &str, Vec<Statement> >, ws!(
    many0!(statement)
));

named!(pub program< &str, Program >, do_parse!(
    body: call!(body) >>
    (Program {
        body: body
    })
));

