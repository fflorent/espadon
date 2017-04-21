#[macro_use]
extern crate nom;

use std::str::{self, FromStr};
use nom::{IResult, ErrorKind, digit};

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Number(f32),
    String(String),
    Boolean(bool)
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
    If {
        test: Expression,
        consequent: Box<Statement>,
        alternate: Option<Box<Statement>>
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

named!(boolean_literal_value< &str, LiteralValue >, alt!(
    tag!("true")  => { |_| LiteralValue::Boolean(true)  } |
    tag!("false") => { |_| LiteralValue::Boolean(false) }
));

// Largely inspired from nom (thanks to geal)
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L23-L48
// FIXME support hexadecimal, octal and binary expressions too
named!(number_literal_value< &str, LiteralValue >, map!(
    pair!(
        opt!(alt!(tag!("+") | tag!("-"))),
        map_res!(
            recognize!(
                alt_complete!(
                    delimited!(digit, tag!("."), opt!(complete!(digit))) |
                    delimited!(opt!(digit), tag!("."), digit)            |
                    digit
                )
            ),
            FromStr::from_str
        )
    ),
    |(sign, abs_value): (Option<&str>, f32)| {
        let value = if sign == Some("-") { -abs_value } else { abs_value };
        LiteralValue::Number(value)
    }
));

fn eat_string(input: &str) -> IResult< &str, &str > {
    if input.len() == 0 {
        return IResult::Incomplete(nom::Needed::Unknown);
    }
    let mut chars = input.chars();

    let separator = match chars.nth(0) {
        sep @ Some('\'') | sep @ Some('"') => sep.unwrap(),
        Some(_) | None => return nom::IResult::Error(error_position!(ErrorKind::Custom(43), input))
    };

    let mut escaped = false;
    let mut idx = 1;
    for item in chars {
        if escaped {
            escaped = false;
        } else {
            match item {
                c if c == separator => return IResult::Done(&input[idx+1..], &input[0..idx+1]),
                '\\' => escaped = true,
                '\n' => return IResult::Incomplete(nom::Needed::Unknown),
                _ => ()
            }
        }
        idx += 1;
    }

    return IResult::Error(error_position!(ErrorKind::Custom(43), input));
}

named!(string_literal_value< &str, LiteralValue >, do_parse!(
    string: call!(eat_string) >>
    (LiteralValue::String(string.to_string()))
));

named!(literal_value< &str, LiteralValue >, alt_complete!(
    number_literal_value |
    null_literal_value |
    boolean_literal_value |
    string_literal_value
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

named!(expression< &str, Expression >, ws!(alt_complete!(
    this_expression |
    literal_expression
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
    if_statement
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{IResult, Needed};

    #[test]
    fn it_parses_declaration_expression() {
        assert_eq!(program("var test;"), IResult::Done("", Program {
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
        assert_eq!(program("var test,\n\t foo;"), IResult::Done("", Program {
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
        assert_eq!(program("var test = this,\n\t foo = null;"), IResult::Done("", Program {
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
        assert_eq!(program("this;"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(Expression::ThisExpression)
            ]
        }));
    }

    #[test]
    fn it_parses_null_literal_expression() {
        assert_eq!(program("null;"), IResult::Done("", Program {
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
    fn it_parses_decimal_number_literal_expression() {
        assert_eq!(program("42;   +42.042;   -42.4242;"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(42.0f32)
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(42.042f32)
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(-42.4242f32)
                    }
                )
            ]
        }));
    }

    // FIXME should pass…
    #[test]
    fn it_parses_octal_number_literal_expression() {
        assert_eq!(program("0o10"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(8f32)
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_string_literal_expression() {
        let res = program("\"foo\";   'foo'; \"foo$\\n \\\"bar\\\"\";");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String("\"foo\"".to_string())
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String("'foo'".to_string())
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::String("\"foo$\\n \\\"bar\\\"\"".to_string())
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_incomplete_string_literal_expression() {
        assert_eq!(program("\"foo   \r\n bar\";"), IResult::Incomplete(Needed::Unknown));
        assert_eq!(program("\"foo   \n bar\";"), IResult::Incomplete(Needed::Unknown));
    }


    #[test]
    fn it_parses_boolean_literal_expression() {
        assert_eq!(program("true; false;"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Boolean(true)
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Boolean(false)
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_if_statement() {
        // FIXME test with newlines in the test. ASI shouldn't work in it.
        // TODO

        assert_eq!(program("if ( true ) {\n 42; \n 43; \n}"), IResult::Done("", Program {
            body: vec![
                Statement::If {
                    test: Expression::Literal {
                        value: LiteralValue::Boolean(true)
                    },
                    consequent: Box::new(Statement::Block {
                        body: vec![
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(42f32)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(43f32)
                                }
                            )
                        ]
                    }),
                    alternate: None
                }
            ]
        }));
    }

    #[test]
    fn it_parses_if_else_statement() {
        // FIXME test with newlines in the test. ASI shouldn't work in it.
        assert_eq!(program("if (true) {\n 42; 43; \n} else { 44; 45; }"), IResult::Done("", Program {
            body: vec![
                Statement::If {
                    test: Expression::Literal {
                        value: LiteralValue::Boolean(true)
                    },
                    consequent: Box::new(Statement::Block {
                        body: vec![
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(42f32)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(43f32)
                                }
                            )
                        ]
                    }),
                    alternate: Some(Box::new(Statement::Block{
                        body: vec![
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(44f32)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(45f32)
                                }
                            )
                        ]
                    }))
                }
            ]
        }));
    }

    #[test]
    fn it_parses_block_statement() {
        assert_eq!(program("{ true; var test; }"), IResult::Done("", Program {
            body: vec![
                Statement::Block {
                    body: vec![
                        Statement::Expression(
                            Expression::Literal {
                                value: LiteralValue::Boolean(true)
                            }
                        ),
                        Statement::VariableDeclaration {
                            declarations: vec![VariableDeclarator {
                                id: "test".to_string(),
                                init: None
                            }],
                            kind: "var".to_string()
                        }
                    ]
                }
            ]
        }));
    }

    #[test]
    fn it_parses_empty_statement() {
        let res = program(";");
        let with_space = program("  ;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::Empty
            ]
        }));
        assert_eq!(with_space, res);
    }

}
