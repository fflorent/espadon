use super::*;
use statements::statement;
use nom::Slice;
use tests::utils::GetLocation;

fn check_statement<'a, F>(statement_to_parse: &'a str, get_expected: F)
    where F: Fn(StrSpan<'a>) -> Statement<'a> {
    let input = StrSpan::new(statement_to_parse);
    let remaining = input.slice(statement_to_parse.len()..);
    assert_eq!(statement(input), IResult::Done(remaining, get_expected(input)));
}

#[test]
fn it_parses_expressions() {
    check_statement("this;", |input| Statement::Expression(Expression::ThisExpression {
        loc: input.get_loc(..";")
    }));
}

#[test]
fn it_parses_declarations() {
    check_statement("var test;", |input| Statement::VariableDeclaration(VariableDeclaration {
        declarations: vec![VariableDeclarator {
            id: "test".to_string(),
            init: None,
            loc: input.get_loc("test"..";")
        }],
        kind: "var".to_string(),
        loc: input.get_loc(..";")
    }));
}

#[test]
fn it_parses_multi_declaration() {
    check_statement("var test,\n\t foo;", |input| Statement::VariableDeclaration(VariableDeclaration {
        declarations: vec![VariableDeclarator {
            id: "test".to_string(),
            init: None,
            loc: input.get_loc("test"..",")
        }, VariableDeclarator {
            id: "foo".to_string(),
            init: None,
            loc: input.get_loc("foo"..";")
        }],
        kind: "var".to_string(),
        loc: input.get_loc(..";")
    }));
}

#[test]
fn it_parses_multi_declaration_expression_with_initialisations() {
    check_statement("var test = this,\n\t foo = null;", |input| Statement::VariableDeclaration(VariableDeclaration{
        declarations: vec![VariableDeclarator {
            id: "test".to_string(),
            init: Some(Expression::ThisExpression {
                loc: input.get_loc("this"..",")
            }),
            loc: input.get_loc("test"..","),
        }, VariableDeclarator {
            id: "foo".to_string(),
            init: Some(Expression::Literal(Literal {
                value: LiteralValue::Null,
                loc: input.get_loc("null"..";")
            })),
            loc: input.get_loc("foo"..";")
        }],
        kind: "var".to_string(),
        loc: input.get_loc(..";")
    }));
}

#[test]
fn it_fails_to_parse_var_names_beginning_with_nums() {
    let res = statement(StrSpan::new("var 1test;"));
    // FIXME it should return a meaningfull error…
    assert!(res.is_err());
}

#[test]
fn it_parses_if_statements() {
    // FIXME test with newlines in the test. ASI shouldn't work in it.
    check_statement("if ( true ) {\n 42; \n 43; \n}", |input| Statement::If {
        test: Expression::Literal(Literal {
            value: LiteralValue::Boolean(true),
            loc: input.get_loc("true".." ")
        }),
        consequent: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(42.0),
                        loc: input.get_loc("42"..";")
                    })
                ),
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(43.0),
                        loc: input.get_loc("43"..";")
                    })
                )
            ],
            loc: input.get_loc("{"..)
        }),
        alternate: None,
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_if_else_statements() {
    check_statement("if (true) {\n 42; 43; \n} else { 44; 45; }", |input| Statement::If {
        test: Expression::Literal(Literal {
            value: LiteralValue::Boolean(true),
            loc: input.get_loc("true"..")")
        }),
        consequent: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(42.0),
                        loc: input.get_loc("42"..";")
                    })
                ),
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(43.0),
                        loc: input.get_loc("43"..";")
                    })
                )
            ],
            loc: input.get_loc("{".." else")
        }),
        alternate: Some(Box::new(Statement::Block{
            body: vec![
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(44.0),
                        loc: input.get_loc("44"..";")
                    })
                ),
                Statement::Expression(
                    Expression::Literal(Literal {
                        value: LiteralValue::Number(45.0),
                        loc: input.get_loc("45"..";")
                    })
                )
            ],
            loc: input.get_loc("{ 44"..)
        })),
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_block_statements() {
    check_statement("{ true; var test; }", |input| Statement::Block {
        body: vec![
            Statement::Expression(
                Expression::Literal(Literal {
                    value: LiteralValue::Boolean(true),
                    loc: input.get_loc("true"..";")
                })
            ),
            Statement::VariableDeclaration(VariableDeclaration {
                declarations: vec![VariableDeclarator {
                    id: "test".to_string(),
                    init: None,
                    loc: input.get_loc("test"..";")
                }],
                kind: "var".to_string(),
                loc: input.get_loc("var"..";")
            })
        ],
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_empty_block_statements() {
    check_statement("{}", |input| Statement::Block {
        body: vec![],
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_empty_statements() {
    check_statement(";", |input| {
        Statement::Empty {
            loc: input.get_loc(..)
        }
    });
}

#[test]
fn it_parses_for_statements_with_init_declaration() {
    check_statement("for (var i = 0; i < 42; i+=1) undefined;", |input| Statement::For {
        loc: input.get_loc(..),
        init: Some(ForInitializer::VariableDeclaration(VariableDeclaration {
            declarations: vec![VariableDeclarator {
                id: "i".to_string(),
                init: Some(Expression::Literal(Literal {
                    value: LiteralValue::Number(0.0),
                    loc: input.get_loc("0"..";")
                })),
                loc: input.get_loc("i = 0"..";")
            }],
            kind: "var".to_string(),
            loc: input.get_loc("var"..";")
        })),
        test: Some(Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            }),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";")
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            }),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Expression(
              Expression::Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              })
        )
    });
}

#[test]
fn it_parses_empty_for_statements() {
    check_statement("for (;;) undefined;", |input| Statement::For {
        loc: input.get_loc(..),
        init: None,
        test: None,
        update: None,
        body: Box::new(Statement::Expression(
            Expression::Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            })
        )
    });
}

#[test]
fn it_parses_for_statements_with_init_expression() {
    check_statement("for (i = 0; i < 42; i+=1) undefined;", |input| Statement::For {
        loc: input.get_loc(..),
        init: Some(ForInitializer::Expression(Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i =".." ")
            }),
            operator: "=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(0.0),
                loc: input.get_loc("0"..";")
            })),
            loc: input.get_loc("i ="..";")
        })),
        test: Some(Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i <".." ")
            }),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";")
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            }),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Expression(
            Expression::Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            })
        )
    });
}

#[test]
fn it_parses_for_statements_with_block_body() {
    check_statement("for (i = 0; i < 42; i+=1) { undefined; }", |input| Statement::For {
        loc: input.get_loc(..),
        init: Some(ForInitializer::Expression(Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i = ".." ")
            }),
            operator: "=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(0.0),
                loc: input.get_loc("0"..";")
            })),
            loc: input.get_loc("i ="..";")
        })),
        test: Some(Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i <".." ")
            }),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";"),
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            }),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                })
            ],
            loc: input.get_loc("{"..)
        })
    });
}

#[test]
fn it_parses_while_statements() {
    check_statement("while (i < 42) undefined;", |input| Statement::While {
        loc: input.get_loc(..),
        test: Box::new(Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            }),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Expression(
              Expression::Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              })
        )
    });
}

#[test]
fn it_parses_while_statements_with_block_body() {
    check_statement("while (i < 42) { undefined; }", |input| Statement::While {
        loc: input.get_loc(..),
        test: Box::new(Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            }),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                })
            ],
            loc: input.get_loc("{"..)
        })
    });
}
