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
    },
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
    return_error!(ErrorKind::Custom(42), peek!(none_of!("0123456789"))) >>
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
    let mut chars = input.char_indices();

    let separator = match chars.nth(0) {
        sep @ Some((_, '\'')) | sep @ Some((_, '"')) => sep.unwrap().1,
        // FIXME meaningfull error codes
        Some(_) | None => return nom::IResult::Error(error_position!(ErrorKind::Custom(43), input))
    };

    let mut escaped = false;
    for (idx, item) in chars {
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

#[cfg(test)]
mod tests {
    use super::*;
    use nom::{IResult, Needed};

    #[test]
    fn it_parses_declaration_expressions() {
        assert_eq!(program("var test;"), IResult::Done("", Program {
            body: vec![
                Statement::VariableDeclaration(VariableDeclaration {
                    declarations: vec![VariableDeclarator {
                        id: "test".to_string(),
                        init: None
                    }],
                    kind: "var".to_string()
                })
            ]
        }));
    }

    #[test]
    fn it_parses_multi_declaration_expressions() {
        assert_eq!(program("var test,\n\t foo;"), IResult::Done("", Program {
            body: vec![
                Statement::VariableDeclaration(VariableDeclaration {
                    declarations: vec![VariableDeclarator {
                        id: "test".to_string(),
                        init: None
                    }, VariableDeclarator {
                        id: "foo".to_string(),
                        init: None
                    }],
                    kind: "var".to_string()
                })
            ]
        }));
    }

    #[test]
    fn it_parses_multi_declaration_expression_with_initialisations() {
        assert_eq!(program("var test = this,\n\t foo = null;"), IResult::Done("", Program {
            body: vec![
                Statement::VariableDeclaration(VariableDeclaration{
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
                })
            ]
        }));
    }

    #[test]
    fn it_parses_statements_with_whitespaces_around() {
        assert_eq!(program(" var test;"), program("var test;"));
        assert_eq!(program(" var   test   ;"), program("var test;"));
        assert_eq!(program(" var   test   ;   ").to_result(), program("var test;").to_result());
    }

    // skip
    // #[test]
    fn it_fails_to_parse_var_names_beginning_with_nums() {
        let res = program("var 1test;");
        println!("FAIL HERE {:?}", res);
        assert!(res.is_err());
        assert!(false, "decris mieux!");
    }

    #[test]
    fn it_parses_this_expressions() {
        assert_eq!(program("this;"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(Expression::ThisExpression)
            ]
        }));
    }

    #[test]
    fn it_parses_null_literal_expressions() {
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
    fn it_parses_decimal_number_literal_expressions() {
        assert_eq!(program("42;   +42.042;   -42.4242;"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(42.0)
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(42.042)
                    }
                ),
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(-42.4242)
                    }
                )
            ]
        }));
    }

    // skip
    // #[test]
    fn it_parses_octal_number_literal_expressions() {
        assert_eq!(program("0o10"), IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Literal {
                        value: LiteralValue::Number(8.0)
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_string_literal_expressions() {
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

    // skip
    // #[test]
    fn it_parses_incomplete_string_literal_expressions() {
        assert_eq!(program("\"foo   \r\n bar\";"), IResult::Incomplete(Needed::Unknown));
        assert_eq!(program("\"foo   \n bar\";"), IResult::Incomplete(Needed::Unknown));
    }


    #[test]
    fn it_parses_boolean_literal_expressions() {
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
    fn it_parses_if_statements() {
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
                                    value: LiteralValue::Number(42.0)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(43.0)
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
    fn it_parses_if_else_statements() {
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
                                    value: LiteralValue::Number(42.0)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(43.0)
                                }
                            )
                        ]
                    }),
                    alternate: Some(Box::new(Statement::Block{
                        body: vec![
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(44.0)
                                }
                            ),
                            Statement::Expression(
                                Expression::Literal {
                                    value: LiteralValue::Number(45.0)
                                }
                            )
                        ]
                    }))
                }
            ]
        }));
    }

    #[test]
    fn it_parses_block_statements() {
        assert_eq!(program("{ true; var test; }"), IResult::Done("", Program {
            body: vec![
                Statement::Block {
                    body: vec![
                        Statement::Expression(
                            Expression::Literal {
                                value: LiteralValue::Boolean(true)
                            }
                        ),
                        Statement::VariableDeclaration(VariableDeclaration {
                            declarations: vec![VariableDeclarator {
                                id: "test".to_string(),
                                init: None
                            }],
                            kind: "var".to_string()
                        })
                    ]
                }
            ]
        }));
    }

    #[test]
    fn it_parses_empty_block_statements() {
        assert_eq!(program("{}"), IResult::Done("", Program {
            body: vec![
                Statement::Block {
                    body: vec![]
                }
            ]
        }));
    }

    #[test]
    fn it_parses_empty_statements() {
        let res = program(";");
        let with_space = program("  ;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::Empty
            ]
        }));
        assert_eq!(with_space, res);
    }

    #[test]
    fn it_parses_identifiers() {
        let res = program("undefined;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::Expression(
                    Expression::Identifier {
                        name: "undefined".to_string()
                    }
                )
            ]
        }));
    }

    #[test]
    fn it_parses_assignment_expressions() {
        let assignment_operators = vec!["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=", "|=", "^=", "&="];
        for assignment_operator in assignment_operators {
            assert_eq!(program(&format!("a {} 42;", assignment_operator)), IResult::Done("", Program {
                body: vec![
                    Statement::Expression (
                        Expression::Assignment {
                            left: Box::new(Expression::Identifier {
                                name: "a".to_string()
                            }),
                            operator: assignment_operator.to_string(),
                            right: Box::new(Expression::Literal {
                                value: LiteralValue::Number(42.0)
                            })
                        }
                    )
                ]
            }), "should parse for {}", assignment_operator);
        }
    }

    #[test]
    fn it_parses_binary_expressions() {
        let binary_operators = vec!["==", "!=", "===", "!==", "<", "<=", ">", ">=", "<<", ">>", ">>>", "+", "-", "*", "/", "%", "|", "^", "&", "in", "instanceof"];
        for binary_operator in binary_operators {
            assert_eq!(program(&format!("a {} 42;", binary_operator)), IResult::Done("", Program {
                body: vec![
                    Statement::Expression (
                        Expression::Binary {
                            left: Box::new(Expression::Identifier {
                                name: "a".to_string()
                            }),
                            operator: binary_operator.to_string(),
                            right: Box::new(Expression::Literal {
                                value: LiteralValue::Number(42.0)
                            })
                        }
                    )
                ]
            }), "should parse for {}", binary_operator);
        }
    }

    #[test]
    fn it_parses_for_statements_with_init_declaration() {
        let res = program("for (var i = 0; i < 42; i+=1) undefined;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::For {
                    init: Some(ForInitializer::VariableDeclaration(VariableDeclaration {
                        declarations: vec![VariableDeclarator {
                            id: "i".to_string(),
                            init: Some(Expression::Literal {
                                value: LiteralValue::Number(0.0)
                            })
                        }],
                        kind: "var".to_string()
                    })),
                    test: Some(Expression::Binary {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "<".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(42.0)
                        })
                    }),
                    update: Some(Expression::Assignment {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "+=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(1.0)
                        })
                    }),
                    body: Box::new(Statement::Expression(
                        Expression::Identifier {
                            name: "undefined".to_string()
                        })
                    )
                }
            ]
        }));
    }

    #[test]
    fn it_parses_empty_for_statements() {
        let res = program("for (;;) undefined;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::For {
                    init: None,
                    test: None,
                    update: None,
                    body: Box::new(Statement::Expression(
                        Expression::Identifier {
                            name: "undefined".to_string()
                        })
                    )
                }
            ]
        }));
    }

    #[test]
    fn it_parses_for_statements_with_init_expression() {
        let res = program("for (i = 0; i < 42; i+=1) undefined;");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::For {
                    init: Some(ForInitializer::Expression(Expression::Assignment {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(0.0)
                        })
                    })),
                    test: Some(Expression::Binary {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "<".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(42.0)
                        })
                    }),
                    update: Some(Expression::Assignment {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "+=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(1.0)
                        })
                    }),
                    body: Box::new(Statement::Expression(
                        Expression::Identifier {
                            name: "undefined".to_string()
                        })
                    )
                }
            ]
        }));
    }

    #[test]
    fn it_parses_for_statements_with_block_body() {
        let res = program("for (i = 0; i < 42; i+=1) { undefined; }");
        assert_eq!(res, IResult::Done("", Program {
            body: vec![
                Statement::For {
                    init: Some(ForInitializer::Expression(Expression::Assignment {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(0.0)
                        })
                    })),
                    test: Some(Expression::Binary {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "<".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(42.0)
                        })
                    }),
                    update: Some(Expression::Assignment {
                        left: Box::new(Expression::Identifier {
                            name: "i".to_string()
                        }),
                        operator: "+=".to_string(),
                        right: Box::new(Expression::Literal {
                            value: LiteralValue::Number(1.0)
                        })
                    }),
                    body: Box::new(Statement::Block {
                        body: vec![
                            Statement::Expression(Expression::Identifier {
                                name: "undefined".to_string()
                            })
                        ]
                    })
                }
            ]
        }));
    }

}
