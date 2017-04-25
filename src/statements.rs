use expressions::{expression, Expression};
use misc::{identifier_name};

/// [A variable declarator]
/// (https://github.com/estree/estree/blob/master/es5.md#variabledeclarator)
#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: String,
    pub init: Option<Expression>
}

/// [A variable declaration]
/// (https://github.com/estree/estree/blob/master/es5.md#variabledeclaration)
#[derive(Debug, PartialEq)]
pub struct VariableDeclaration {
    pub declarations: Vec<VariableDeclarator>,
    pub kind: String
}

/// A [for statement][for_statement] initializer.
/// [for_statement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
#[derive(Debug, PartialEq)]
pub enum ForInitializer {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression)
}

/// [A statement]
/// (https://github.com/estree/estree/blob/master/es5.md#statements)
#[derive(Debug, PartialEq)]
pub enum Statement {

    // FIXME either there is something smarted to do, or we should go for generalize it
    // everywhere
    VariableDeclaration(VariableDeclaration),
    /// [A if statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#ifstatement)
    If {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>
    },

    /// [A for statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#forstatement)
    For {
        init: Option<ForInitializer>,
        test: Option<Expression>,
        update: Option<Expression>,
        body: Box<Statement>
    },

    /// [A block statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#blockstatement)
    Block {
        body: Vec<Statement>
    },

    /// [An empty statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#emptystatement)
    Empty,

    /// [An expression statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#expressionstatement)
    Expression(Expression)
}

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
        call!(statement_list),
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

named!(expression_statement< &str, Statement >, do_parse!(
    expression: call!(expression) >>
    (Statement::Expression(expression))
));

/// Statement parser
named!(pub statement< &str, Statement >, alt_complete!(
    empty_statement |
    terminated!(variable_declaration_statement, tag!(";")) |
    terminated!(expression_statement, tag!(";")) |
    block_statement |
    if_statement |
    for_statement
));

/// Statement list parser
named!(pub statement_list< &str, Vec<Statement> >, ws!(
    many0!(statement)
));

