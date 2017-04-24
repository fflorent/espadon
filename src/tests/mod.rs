use super::*;
use super::literals::LiteralValue;
use nom::{IResult, Needed};

mod literals;

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
                    init: Some(Expression::Literal(Literal {
                        value: LiteralValue::Null
                    }))
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
fn it_parses_literal_expressions() {
    assert_eq!(literal_expression("true"), IResult::Done("", Expression::Literal(Literal {
        value: LiteralValue::Boolean(true)
    })));
}

#[test]
fn it_parses_if_statements() {
    // FIXME test with newlines in the test. ASI shouldn't work in it.
    // TODO

    assert_eq!(program("if ( true ) {\n 42; \n 43; \n}"), IResult::Done("", Program {
        body: vec![
            Statement::If {
                test: Expression::Literal(Literal {
                    value: LiteralValue::Boolean(true)
                }),
                consequent: Box::new(Statement::Block {
                    body: vec![
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(42.0)
                            })
                        ),
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(43.0)
                            })
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
                test: Expression::Literal(Literal {
                    value: LiteralValue::Boolean(true)
                }),
                consequent: Box::new(Statement::Block {
                    body: vec![
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(42.0)
                            })
                        ),
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(43.0)
                            })
                        )
                    ]
                }),
                alternate: Some(Box::new(Statement::Block{
                    body: vec![
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(44.0)
                            })
                        ),
                        Statement::Expression(
                            Expression::Literal(Literal {
                                value: LiteralValue::Number(45.0)
                            })
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
                        Expression::Literal(Literal {
                            value: LiteralValue::Boolean(true)
                        })
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
                        right: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Number(42.0)
                        }))
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
                        right: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Number(42.0)
                        }))
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
                        init: Some(Expression::Literal(Literal {
                            value: LiteralValue::Number(0.0)
                        }))
                    }],
                    kind: "var".to_string()
                })),
                test: Some(Expression::Binary {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "<".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(42.0)
                    }))
                }),
                update: Some(Expression::Assignment {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "+=".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(1.0)
                    }))
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
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(0.0)
                    }))
                })),
                test: Some(Expression::Binary {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "<".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(42.0)
                    }))
                }),
                update: Some(Expression::Assignment {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "+=".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(1.0)
                    }))
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
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(0.0)
                    }))
                })),
                test: Some(Expression::Binary {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "<".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(42.0)
                    }))
                }),
                update: Some(Expression::Assignment {
                    left: Box::new(Expression::Identifier {
                        name: "i".to_string()
                    }),
                    operator: "+=".to_string(),
                    right: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Number(1.0)
                    }))
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
