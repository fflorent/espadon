use super::literals::{Literal, literal};
pub use super::misc::{Identifier, identifier, StrSpan, Location};

#[derive(Debug, PartialEq)]
/// [An Expression]
/// (https://github.com/estree/estree/blob/master/es5.md#expressionstatement)
pub enum Expression<'a> {

    /// [A this expression]
    /// (https://github.com/estree/estree/blob/master/es5.md#thisexpression)
    ///
    /// ```ignore
    /// assert_eq!(expression("this"), IResult::Done("", Expression::This));
    /// ```
    ThisExpression {
        loc: Location<'a>,
    },

    /// [A literal expression]
    /// (https://github.com/estree/estree/blob/master/es5.md#literal)
    ///
    /// ```ignore
    /// assert_eq!(expression("42"), IResult::Done("", Expression::Literal(Literal {
    ///     value: LiteralValue::Number(42.0)
    /// })));
    /// ```
    Literal(Literal<'a>),

    /// [An assignment expression]
    /// (https://github.com/estree/estree/blob/master/es5.md#assignmentexpression)
    ///
    ///
    /// ```ignore
    /// assert_eq!(expression("a = 42"), IResult::Done("", Expression::Assignment {
    ///     left: Box::new(Expression::Identifier(Identifier {
    ///         name: "a".to_string()
    ///     })),
    ///     operator: "=".to_string(),
    ///     right: Box::new(Expression::Literal(Literal {
    ///         value: LiteralValue::Number(42.0)
    ///     }))
    /// }));
    /// ```
    Assignment {
        left: Box<Expression<'a>>,
        operator: String,
        right: Box<Expression<'a>>,
        loc: Location<'a>
    },

    /// [A binary expression]
    /// (https://github.com/estree/estree/blob/master/es5.md#binaryexpression)
    ///
    ///
    /// ```ignore
    /// assert_eq!(expression("a == 42"), IResult::Done("", Expression::Binary {
    ///     left: Box::new(Expression::Identifier(Identifier {
    ///         name: "a".to_string()
    ///     })),
    ///     operator: "==".to_string(),
    ///     right: Box::new(Expression::Literal(Literal {
    ///         value: LiteralValue::Number(42.0)
    ///     }))
    /// }));
    /// ```
    Binary {
        left: Box<Expression<'a>>,
        operator: String,
        right: Box<Expression<'a>>,
        loc: Location<'a>
    },

    /// [An identifier]
    /// (https://github.com/estree/estree/blob/master/es5.md#identifier)
    ///
    /// ```ignore
    /// assert_eq!(expression("foo"), IResult::Done("", Expression::Identifier(Identifier {
    ///     name: "foo".to_string()
    /// })));
    /// ```
    Identifier (Identifier<'a>),

    /// [An array expression]
    /// (https://github.com/estree/estree/blob/master/es5.md#arrayexpression)
    ///
    /// ```ignore
    /// assert_eq!(expression("[foo, a = b, 42]", |input| Expression::Array {
    ///     elements: vec![
    ///         Expression::Identifier(Identifier {
    ///             name: "foo".to_string(),
    ///             loc: input.get_loc("foo"..",")
    ///         }),
    ///         Expression::Assignment {
    ///             left: Box::new(Expression::Identifier(Identifier {
    ///                 name: "a".to_string(),
    ///                 loc: input.get_loc("a".." ")
    ///             })),
    ///             operator: "=".to_string(),
    ///             right: Box::new(Expression::Identifier(Identifier {
    ///                 name: "b".to_string(),
    ///                 loc: input.get_loc("b"..",")
    ///             })),
    ///             loc: input.get_loc("a"..",")
    ///         },
    ///         Expression::Literal(Literal {
    ///             value: LiteralValue::Number(42.0),
    ///             loc: input.get_loc("42".."]")
    ///         }),
    ///     ],
    ///     loc: input.get_loc(..)
    /// });
    /// ```
    Array {
        elements: Vec<Expression<'a>>,
        loc: Location<'a>
    }
}

named!(literal_expression< StrSpan, Expression >, map!(
    literal,
    |literal| (Expression::Literal(literal))
));

named!(this_expression< StrSpan, Expression >, es_parse!({
        tag!("this")
    } => (Expression::ThisExpression {})
));

named!(assignment_operators< StrSpan, StrSpan >, ws!(alt_complete!(
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

named!(assignment_expression< StrSpan, Expression >, es_parse!({
        left: identifier_expression >>
        operator: assignment_operators >>
        right: expression_without_ws
    } => (Expression::Assignment {
        left: Box::new(left),
        operator: operator.to_string(),
        right: Box::new(right)
    })
));

named!(binary_operators< StrSpan, StrSpan >, ws!(alt_complete!(
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

named!(binary_expression< StrSpan, Expression >, es_parse!({
        left: identifier_expression >>
        operator: binary_operators >>
        right: expression
    } => (Expression::Binary {
        left: Box::new(left),
        operator: operator.to_string(),
        right: Box::new(right)
    })
));


named!(identifier_expression < StrSpan, Expression >, map!(
    identifier,
    |identifier| Expression::Identifier(identifier)
));

named!(array_expression< StrSpan, Expression >, es_parse!({
        elements: delimited!(
            tag!("["),
            separated_list!(tag!(","), expression),
            tag!("]")
        )
    } => (Expression::Array {
        elements: elements
    })
));

named!(pub expression_without_ws< StrSpan, Expression >, alt_complete!(
    this_expression |
    literal_expression |
    assignment_expression |
    binary_expression |
    identifier_expression |
    array_expression
));

named!(pub expression< StrSpan, Expression >, ws!(expression_without_ws));

