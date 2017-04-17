#[macro_use]
extern crate nom;

use std::str;
use nom::{IResult, ErrorKind, digit};

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Number,
    String
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    ThisExpression,
    Literal {
        value: LiteralValue
    }
}

#[derive(Debug, PartialEq)]
pub struct VariableDeclarator {
    pub id: String,
    pub init: Option<Expression>
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    VariableDeclaration {
        declarations: Vec<VariableDeclarator>,
        kind: String
    },
    Expression(Expression)
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>
}

fn var_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

named!(variable_declarator< &str, VariableDeclarator >, do_parse!(
    return_error!(ErrorKind::Custom(42), peek!(none_of!("0123456789"))) >>
    // return_error!(ErrorKind::Custom(42), not!(tag!("1"))) >>
    // peek!(verify!(nom::anychar, |c: char| (c.is_alphabetic() || c == '_' || c == '$'))) >>
    id: take_while1_s!(var_name_char) >>
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

named!(variable_declaration_statement< &str, Statement >, do_parse!(
    var_type: tag!("var") >>
    take_while1_s!(char::is_whitespace) >>
    declarators: ws!(separated_list!(tag!(","), variable_declarator)) >>
    (Statement::VariableDeclaration {
        declarations: declarators,
        kind: var_type.to_string()
    })
));

named!(null_literal_value< &str, LiteralValue >, do_parse!(
    tag!("null") >>
    (LiteralValue::Null)
));

// Largely inspired from nom (thanks to geal)
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L23-L48
named!(number_literal_value< &str, LiteralValue >, do_parse!(
    pair!(
        opt!(alt!(tag!("+") | tag!("-"))),
        alt_complete!(
            delimited!(digit, tag!("."), opt!(complete!(digit))) |
            delimited!(opt!(digit), tag!("."), digit)            |
            digit
        )
    ) >>
    (LiteralValue::Number)
));

// Largely inspired from nom (thanks to geal)
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L51-57
// named!(string_literal_value< &str, LiteralValue >, do_parse!(
//     delimited!(
//         tag!("\""),
//         escaped!(call!(nom::alphanumeric)
//
// ));

named!(literal_value< &str, LiteralValue >, alt_complete!(
    null_literal_value |
    number_literal_value
));

named!(literal_expression< &str, Expression >, do_parse!(
    literal_value: call!(literal_value) >>
    (Expression::Literal {
        value: literal_value
    })
));

named!(this_expression< &str, Expression >, do_parse!(
    tag!("this") >>
    (Expression::ThisExpression)
));

named!(expression< &str, Expression >, alt_complete!(
    this_expression |
    literal_expression
));

named!(expression_statement< &str, Statement >, do_parse!(
    expression: call!(expression) >>
    (Statement::Expression(expression))
));

named!(statement< &str, Statement >, alt_complete!(
    variable_declaration_statement |
    expression_statement
));

named!(pub program< &str, Program >, do_parse!(
    // TODO support ASI
    body: ws!(separated_list!(tag!(";"), statement)) >>
    (Program {
        body: body
    })
));

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_declaration_expression() {
        assert_eq!(program("var test;").to_result(), Ok(Program {
            body: vec![
                Statement::VariableDeclaration {
                    declarations: vec![VariableDeclarator {
                        id: "test".to_string(),
                        init: None
                    }],
                    kind: "var".to_string()
                }
            ]
        }));
    }

    #[test]
    fn it_parses_multi_declaration_expression() {
        assert_eq!(program("var test,\n\t foo;").to_result(), Ok(Program {
            body: vec![
                Statement::VariableDeclaration {
                    declarations: vec![VariableDeclarator {
                        id: "test".to_string(),
                        init: None
                    }, VariableDeclarator {
                        id: "foo".to_string(),
                        init: None
                    }],
                    kind: "var".to_string()
                }
            ]
        }));
    }

    #[test]
    fn it_parses_multi_declaration_expression_with_initialisation() {
        assert_eq!(program("var test = this,\n\t foo = null;").to_result(), Ok(Program {
            body: vec![
                Statement::VariableDeclaration {
                    declarations: vec![VariableDeclarator {
                        id: "test".to_string(),
                        init: Some(Expression::ThisExpression)
                    }, VariableDeclarator {
                        id: "foo".to_string(),
                        init: Some(Expression::Literal {
                            value: LiteralValue::Null
                        })
                    }],
                    kind: "var".to_string()
                }
            ]
        }));
    }

    #[test]
    fn it_parses_statements_with_whitespaces_around() {
        assert_eq!(program(" var test;"), program("var test;"));
        assert_eq!(program(" var   test   ;"), program("var test;"));
        assert_eq!(program(" var   test   ;   ").to_result(), program("var test;").to_result());
    }

    #[test]
    fn it_fails_to_parse_var_names_beginning_with_nums() {
        let res = program("var 1test;");
        println!("FAIL HERE {:?}", res);
        assert!(res.is_err());
        assert!(false, "decris mieux!");
    }

    #[test]
    fn it_parses_this_expression() {
        assert_eq!(program("this;").to_result(), Ok(Program {
            body: vec![
                Statement::Expression(Expression::ThisExpression)
            ]
        }));
    }

    #[test]
    fn it_parses_null_literal_expression() {
        assert_eq!(program("null;").to_result(), Ok(Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Null
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_number_literal_expression() {
        assert_eq!(program("42;   +42.042;   -42.4242;").to_result(), Ok(Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_string_literal_expression() {
        assert_eq!(program("\"foo\";   'foo'; \"foo$ \\\"bar\\\"\";").to_result(), Ok(Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String
                    }
                )
            ]
        }));
    }

}
