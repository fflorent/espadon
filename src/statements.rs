use expressions::{expression, Expression};
use misc::{identifier_name, StrSpan, Location};

/// [A variable declarator]
/// (https://github.com/estree/estree/blob/master/es5.md#variabledeclarator)
#[derive(Debug, PartialEq)]
pub struct VariableDeclarator<'a> {
    pub id: String,
    pub init: Option<Expression<'a>>,
    pub loc: Location<'a>,
}

/// [A variable declaration]
/// (https://github.com/estree/estree/blob/master/es5.md#variabledeclaration)
#[derive(Debug, PartialEq)]
pub struct VariableDeclaration<'a> {
    pub declarations: Vec<VariableDeclarator<'a>>,
    pub kind: String,
    pub loc: Location<'a>
}

/// A [for statement][for_statement] initializer.
/// [for_statement]: https://github.com/estree/estree/blob/master/es5.md#forstatement
#[derive(Debug, PartialEq)]
pub enum ForInitializer<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
    Expression(Expression<'a>)
}

/// [A statement]
/// (https://github.com/estree/estree/blob/master/es5.md#statements)
#[derive(Debug, PartialEq)]
pub enum Statement<'a> {

    // FIXME either there is something smarted to do, or we should go for generalize it
    // everywhere
    VariableDeclaration(VariableDeclaration<'a>),
    /// [A if statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#ifstatement)
    If {
        test: Expression<'a>,
        consequent: Box<Statement<'a>>,
        alternate: Option<Box<Statement<'a>>>,
        loc: Location<'a>
    },

    /// [A for statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#forstatement)
    For {
        init: Option<ForInitializer<'a>>,
        test: Option<Expression<'a>>,
        update: Option<Expression<'a>>,
        body: Box<Statement<'a>>,
        loc: Location<'a>,
    },

    /// [A while statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#whilestatement)
    While {
        test: Box<Expression<'a>>,
        body: Box<Statement<'a>>,
        loc: Location<'a>
    },

    /// [A do / while statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#dowhilestatement)
    DoWhile {
        body: Box<Statement<'a>>,
        test: Box<Expression<'a>>,
        loc: Location<'a>
    },

    /// [A block statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#blockstatement)
    Block {
        body: Vec<Statement<'a>>,
        loc: Location<'a>
    },

    /// [An empty statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#emptystatement)
    Empty {
        loc: Location<'a>
    },

    /// [An expression statement]
    /// (https://github.com/estree/estree/blob/master/es5.md#expressionstatement)
    Expression(Expression<'a>)
}

named!(variable_declarator< StrSpan, VariableDeclarator >, es_parse!({
        id: identifier_name >>
        init: opt!(
            do_parse!(
                ws!(tag!("=")) >>
                res: expression >>
                (res)
            )
        )
    } => (VariableDeclarator {
        id: id.to_string(),
        init: init
    })
));

named!(for_statement< StrSpan, Statement >, es_parse!({
        ws!(tag!("for")) >>
        ws!(tag!("(")) >>
        init: alt!(
            variable_declaration    => { |var_decl| (Some(ForInitializer::VariableDeclaration(var_decl))) } |
            expression              => { |expr| (Some(ForInitializer::Expression(expr))) } |
            tag!("")                => { |_| (None) }
        ) >>
        ws!(tag!(";")) >>
        test: opt!(expression) >>
        ws!(tag!(";")) >>
        update: opt!(expression) >>
        ws!(tag!(")")) >>
        body: statement
    } => (Statement::For {
        init: init,
        test: test,
        update: update,
        body: Box::new(body)
    })
));

named!(while_statement< StrSpan, Statement >, es_parse!({
        ws!(tag!("while")) >>
        test: ws!(delimited!(
            tag!("("),
            expression,
            tag!(")")
        )) >>
        body: statement
    } => (Statement::While {
        test: Box::new(test),
        body: Box::new(body)
    })
));

named!(do_while_statement< StrSpan, Statement >, es_parse!({
        ws!(tag!("do")) >>
        body: statement >>
        ws!(tag!("while")) >>
        test: ws!(delimited!(
            tag!("("),
            expression,
            tag!(")")
        ))
    } => (Statement::DoWhile {
        body: Box::new(body),
        test: Box::new(test)
    })
));


named!(if_statement< StrSpan, Statement >, es_parse!({
        ws!(tag!("if")) >>
        test: delimited!(tag!("("), expression, tag!(")")) >>
        consequent: call!(block_statement) >>
        // opt!(complete!(…)) so it doesn't return an incomplete state
        // FIXME: isn't there a smarter way to handle this?
        alternate_opt: opt!(complete!(preceded!(ws!(tag!("else")), block_statement)))
    } => (Statement::If {
        test: test,
        consequent: Box::new(consequent),
        alternate: match alternate_opt {
            Some(alternate) => Some(Box::new(alternate)),
            None => None
        }
    })
));

named!(empty_statement< StrSpan, Statement >, es_parse!(
    {
        tag!(";")
    } => (Statement::Empty {})
));

named!(block_statement< StrSpan, Statement >, ws!(es_parse!({
        body: delimited!(
            tag!("{"),
            call!(statement_list),
            tag!("}")
        )
    } => (Statement::Block { body: body })
)));


named!(variable_declaration< StrSpan, VariableDeclaration >, es_parse!(
    {
        var_type: tag!("var") >>
        take_while1_s!(char::is_whitespace) >>
        declarators: ws!(separated_list!(tag!(","), variable_declarator))
    } => (VariableDeclaration {
        declarations: declarators,
        kind: var_type.to_string(),
    })
));

named!(variable_declaration_statement< StrSpan, Statement >, map!(
    variable_declaration,
    |var_decl| (Statement::VariableDeclaration(var_decl))
));

named!(expression_statement< StrSpan, Statement >, do_parse!(
    expression: call!(expression) >>
    (Statement::Expression(expression))
));

/// Statement parser
named!(pub statement< StrSpan, Statement >, alt_complete!(
    empty_statement |
    terminated!(variable_declaration_statement, tag!(";")) |
    terminated!(expression_statement, tag!(";")) |
    block_statement |
    if_statement |
    for_statement |
    while_statement |
    terminated!(do_while_statement, opt2!(tag!(";")))
));

/// Statement list parser
named!(pub statement_list< StrSpan, Vec<Statement> >, ws!(
    many0!(statement)
));
