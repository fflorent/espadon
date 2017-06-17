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
            id: Identifier {
                name: "test".to_string(),
                loc: input.get_loc("test"..";")
            },
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
            id: Identifier {
                name: "test".to_string(),
                loc: input.get_loc("test"..",")
            },
            init: None,
            loc: input.get_loc("test"..",")
        }, VariableDeclarator {
            id: Identifier {
                name: "foo".to_string(),
                loc: input.get_loc("foo"..";")
            },
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
            id: Identifier {
                name: "test".to_string(),
                loc: input.get_loc("test".." ")
            },
            init: Some(Expression::ThisExpression {
                loc: input.get_loc("this"..",")
            }),
            loc: input.get_loc("test"..","),
        }, VariableDeclarator {
            id: Identifier {
                name: "foo".to_string(),
                loc: input.get_loc("foo".." ")
            },
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
                    id: Identifier {
                        name: "test".to_string(),
                        loc: input.get_loc("test"..";")
                    },
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
                id: Identifier {
                    name: "i".to_string(),
                    loc: input.get_loc("i".." ")
                },
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
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";")
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            })),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Expression(
              Expression::Identifier(Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              }))
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
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });
}

#[test]
fn it_parses_for_statements_with_init_expression() {
    check_statement("for (i = 0; i < 42; i+=1) undefined;", |input| Statement::For {
        loc: input.get_loc(..),
        init: Some(ForInitializer::Expression(Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i =".." ")
            })),
            operator: "=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(0.0),
                loc: input.get_loc("0"..";")
            })),
            loc: input.get_loc("i ="..";")
        })),
        test: Some(Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i <".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";")
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            })),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Expression(
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });
}

#[test]
fn it_parses_for_statements_with_block_body() {
    check_statement("for (i = 0; i < 42; i+=1) { undefined; }", |input| Statement::For {
        loc: input.get_loc(..),
        init: Some(ForInitializer::Expression(Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i = ".." ")
            })),
            operator: "=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(0.0),
                loc: input.get_loc("0"..";")
            })),
            loc: input.get_loc("i ="..";")
        })),
        test: Some(Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i <".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";"),
            })),
            loc: input.get_loc("i <"..";")
        }),
        update: Some(Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i+=".."+")
            })),
            operator: "+=".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(1.0),
                loc: input.get_loc("1"..")")
            })),
            loc: input.get_loc("i+="..")")
        }),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier(Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                }))
            ],
            loc: input.get_loc("{"..)
        })
    });
}

#[test]
fn it_parses_for_in_statements_with_init_declaration_and_assignment() {
    // Yeah, WTF…
    check_statement("for (var i = 0 in foo) undefined;", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::VariableDeclaration(VariableDeclaration {
            declarations: vec![VariableDeclarator {
                id: Identifier {
                    name: "i".to_string(),
                    loc: input.get_loc("i".." ")
                },
                init: Some(Expression::Literal(Literal {
                    value: LiteralValue::Number(0.0),
                    loc: input.get_loc("0".." in")
                })),
                loc: input.get_loc("i = 0".." in")
            }],
            kind: "var".to_string(),
            loc: input.get_loc("var".." in")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Expression(
              Expression::Identifier(Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              }))
        )
    });
}

#[test]
fn it_ensures_that_for_in_requires_space_around_in() {
    // testing contains "in" in the identifier. We ensure that the in token requires whitespaces
    check_statement("for (var testing    in     foo) undefined;", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::VariableDeclaration(VariableDeclaration {
            declarations: vec![VariableDeclarator {
                id: Identifier {
                    name: "testing".to_string(),
                    loc: input.get_loc("testing".." ")
                },
                init: None,
                loc: input.get_loc("testing".." ")
            }],
            kind: "var".to_string(),
            loc: input.get_loc("var".."    in")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Expression(
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });

    check_statement("for (testing    in     foo) undefined;", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::Identifier(Identifier {
            name: "testing".to_string(),
            loc: input.get_loc("testing".." ")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Expression(
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });

    {
        let incomplete_for_in = StrSpan::new("for (testing infoo) undefined;");
        assert!(statement(incomplete_for_in).is_err());
    }

    {
        let incomplete_for_in = StrSpan::new("for (var testing = 0in foo) undefined;");
        println!("{:?}", statement(incomplete_for_in));
        assert!(statement(incomplete_for_in).is_err());
    }
}

#[test]
fn it_parses_for_in_statements_with_init_declaration() {
    check_statement("for (var i in foo) undefined;", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::VariableDeclaration(VariableDeclaration {
            declarations: vec![VariableDeclarator {
                id: Identifier {
                    name: "i".to_string(),
                    loc: input.get_loc("i".." ")
                },
                init: None,
                loc: input.get_loc("i".." in")
            }],
            kind: "var".to_string(),
            loc: input.get_loc("var".." in")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Expression(
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });
}

#[test]
fn it_parses_for_in_statements() {
    check_statement("for (i in foo) undefined;", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::Identifier(Identifier {
            name: "i".to_string(),
            loc: input.get_loc("i".." in")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Expression(
            Expression::Identifier(Identifier {
                name: "undefined".to_string(),
                loc: input.get_loc("undefined"..";")
            }))
        )
    });
}

#[test]
fn it_parses_for_in_statements_with_block_body() {
    check_statement("for (i in foo) { undefined; }", |input| Statement::ForIn {
        loc: input.get_loc(..),
        left: Box::new(ForInInitializer::Identifier(Identifier {
            name: "i".to_string(),
            loc: input.get_loc("i".." in")
        })),
        right: Box::new(Expression::Identifier(Identifier {
            name: "foo".to_string(),
            loc: input.get_loc("foo"..")")
        })),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier(Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                }))
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
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Expression(
              Expression::Identifier(Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              }))
        )
    });
}

#[test]
fn it_parses_while_statements_with_block_body() {
    check_statement("while (i < 42) { undefined; }", |input| Statement::While {
        loc: input.get_loc(..),
        test: Box::new(Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier(Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                }))
            ],
            loc: input.get_loc("{"..)
        })
    });
}

#[test]
fn it_parses_do_while_statements() {
    check_statement("do undefined; while (i < 42)", |input| Statement::DoWhile {
        loc: input.get_loc(..),
        test: Box::new(Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Expression(
              Expression::Identifier(Identifier {
                  name: "undefined".to_string(),
                  loc: input.get_loc("undefined"..";")
              }))
        )
    });
}

#[test]
fn it_parses_do_while_statements_with_block_body() {
    check_statement("do { undefined; } while (i < 42)", |input| Statement::DoWhile {
        loc: input.get_loc(..),
        test: Box::new(Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "i".to_string(),
                loc: input.get_loc("i < ".." ")
            })),
            operator: "<".to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..")")
            })),
            loc: input.get_loc("i <"..")")
        }),
        body: Box::new(Statement::Block {
            body: vec![
                Statement::Expression(Expression::Identifier(Identifier {
                    name: "undefined".to_string(),
                    loc: input.get_loc("undefined"..";")
                }))
            ],
            loc: input.get_loc("{".." while")
        })
    });
}

#[test]
fn it_parses_do_while_statements_with_semicolon() {
    let statement_without_semicolon =
        String::from("do { undefined; } while (i < 42)");
    let statement_with_semicolon = statement_without_semicolon.clone() + ";";

    let parsed_with = statement(StrSpan::new(&statement_with_semicolon));
    let parsed_without = statement(StrSpan::new(&statement_without_semicolon));

    match parsed_with {
        IResult::Done(remaining, parsed_with_result) => {
            assert_eq!(parsed_with_result, parsed_without.unwrap().1);
            assert_eq!(remaining, StrSpan {
                offset: 33,
                line: 1,
                fragment: ""
            });
        },
        other => {
            panic!("should parse successfully {:?}", other);
        }
    }
}

#[test]
fn it_ignores_whitespaces() {
    check_statement(" this; ", |input| Statement::Expression(Expression::ThisExpression {
        loc: input.get_loc("t"..";")
    }));
}
