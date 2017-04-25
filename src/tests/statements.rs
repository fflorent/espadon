use super::*;

fn check_statement(statement_to_parse: &str, exprected_statement: Statement) {
    assert_eq!(statement(statement_to_parse), IResult::Done("", exprected_statement));
}

#[test]
fn it_parses_expressions() {
    check_statement("this;", Statement::Expression(Expression::ThisExpression));
}

#[test]
fn it_parses_declarations() {
    check_statement("var test;", Statement::VariableDeclaration(VariableDeclaration {
        declarations: vec![VariableDeclarator {
            id: "test".to_string(),
            init: None
        }],
        kind: "var".to_string()
    }));
}

#[test]
fn it_parses_multi_declaration() {
    check_statement("var test,\n\t foo;", Statement::VariableDeclaration(VariableDeclaration {
        declarations: vec![VariableDeclarator {
            id: "test".to_string(),
            init: None
        }, VariableDeclarator {
            id: "foo".to_string(),
            init: None
        }],
        kind: "var".to_string()
    }));
}

#[test]
fn it_parses_multi_declaration_expression_with_initialisations() {
    check_statement("var test = this,\n\t foo = null;", Statement::VariableDeclaration(VariableDeclaration{
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
    }));
}

// skip
// #[test]
fn it_fails_to_parse_var_names_beginning_with_nums() {
    let res = statement("var 1test;");
    println!("FAIL HERE {:?}", res);
    assert!(res.is_err());
    assert!(false, "describe more!");
}


#[test]
fn it_parses_if_statements() {
    // FIXME test with newlines in the test. ASI shouldn't work in it.
    check_statement("if ( true ) {\n 42; \n 43; \n}", Statement::If {
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
    });
}

#[test]
fn it_parses_if_else_statements() {
    check_statement("if (true) {\n 42; 43; \n} else { 44; 45; }", Statement::If {
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
    });
}

#[test]
fn it_parses_block_statements() {
    check_statement("{ true; var test; }", Statement::Block {
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
    });
}

#[test]
fn it_parses_empty_block_statements() {
    check_statement("{}", Statement::Block {
        body: vec![]
    });
}

#[test]
fn it_parses_empty_statements() {
    assert_eq!(statement(";"), IResult::Done("", Statement::Empty));
}

#[test]
fn it_parses_for_statements_with_init_declaration() {
    check_statement("for (var i = 0; i < 42; i+=1) undefined;", Statement::For {
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
    });
}

#[test]
fn it_parses_empty_for_statements() {
    check_statement("for (;;) undefined;", Statement::For {
        init: None,
        test: None,
        update: None,
        body: Box::new(Statement::Expression(
            Expression::Identifier {
                name: "undefined".to_string()
            })
        )
    });
}

#[test]
fn it_parses_for_statements_with_init_expression() {
    check_statement("for (i = 0; i < 42; i+=1) undefined;", Statement::For {
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
    });
}

#[test]
fn it_parses_for_statements_with_block_body() {
    check_statement("for (i = 0; i < 42; i+=1) { undefined; }", Statement::For {
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
    });
}
