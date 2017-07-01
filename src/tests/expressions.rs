use super::*;
use expressions::{expression, Property, PropertyKey};
use nom::Slice;
use tests::utils::GetLocation;

fn check_expression<'a, F>(expression_to_parse: &'a str, get_expected: F)
    where F: Fn(StrSpan<'a>) -> Expression<'a> {
    let input = StrSpan::new(expression_to_parse);
    let remaining = input.slice(expression_to_parse.len()..);
    assert_eq!(expression(input), IResult::Done(remaining, get_expected(input)));
}

#[test]
fn it_parses_literal_expressions() {
    check_expression("true", |input| Expression::Literal(Literal {
        value: LiteralValue::Boolean(true),
        loc: input.get_loc(..)
    }));
}

#[test]
fn it_parses_this_expressions() {
    check_expression("this", |input| Expression::ThisExpression {
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_identifiers() {
    check_expression("undefined", |input| Expression::Identifier(Identifier {
        name: "undefined".to_string(),
        loc: input.get_loc(..)
    }));
}

#[test]
fn it_parses_assignment_expressions() {
    let assignment_operators = vec![
        "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=",
        "|=", "^=", "&="
    ];

    for assignment_operator in assignment_operators {
        let expression_to_parse = format!("a {} 42", assignment_operator);

        check_expression(&expression_to_parse, |input| Expression::Assignment {
            left: Box::new(Expression::Identifier(Identifier {
                name: "a".to_string(),
                loc: input.get_loc("a".." ")
            })),
            operator: assignment_operator.to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..)
            })),
            loc: input.get_loc(..)
        });
    }
}

#[test]
fn it_parses_binary_expressions() {
    let binary_operators = vec![
        "==", "!=", "===", "!==", "<", "<=", ">", ">=", "<<", ">>", ">>>",
        "+", "-", "*", "/", "%", "|", "^", "&", "in", "instanceof"
    ];

    for binary_operator in binary_operators {
        let expression_to_parse = format!("a {} 42", binary_operator);

        check_expression(&expression_to_parse, |input| Expression::Binary {
            left: Box::new(Expression::Identifier(Identifier {
                name: "a".to_string(),
                loc: input.get_loc("a".." ")
            })),
            operator: binary_operator.to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..)

            })),
            loc: input.get_loc(..)
        });
    }
}

#[test]
fn it_parses_array_expressions() {
    check_expression("[]", |input| Expression::Array {
        elements: vec![],
        loc: input.get_loc(..)
    });

    check_expression("[foo, a = b, 42]", |input| Expression::Array {
        elements: vec![
            Expression::Identifier(Identifier {
                name: "foo".to_string(),
                loc: input.get_loc("foo"..",")
            }),
            Expression::Assignment {
                left: Box::new(Expression::Identifier(Identifier {
                    name: "a".to_string(),
                    loc: input.get_loc("a".." ")
                })),
                operator: "=".to_string(),
                right: Box::new(Expression::Identifier(Identifier {
                    name: "b".to_string(),
                    loc: input.get_loc("b"..",")
                })),
                loc: input.get_loc("a"..",")
            },
            Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42".."]")
            }),
        ],
        loc: input.get_loc(..)
    });
}

#[test]
fn it_parses_object_expressions() {
    check_expression("{}", |input| Expression::Object {
        properties: vec![],
        loc: input.get_loc(..)
    });

    check_expression("{foo: 'bar'}", |input| Expression::Object {
        properties: vec![
            Property {
                key: PropertyKey::Identifier(Identifier {
                    name: "foo".to_string(),
                    loc: input.get_loc("foo"..":"),
                }),
                value: Expression::Literal(Literal {
                    value: LiteralValue::String("'bar'".to_string()),
                    loc: input.get_loc("'bar'".."}"),
                }),
                kind: "init".to_string(),
                loc: input.get_loc("foo".."}")
            }
        ],
        loc: input.get_loc(..)
    });

    check_expression("{42: 'bar'}", |input| Expression::Object {
        properties: vec![
            Property {
                key: PropertyKey::Literal(Literal {
                    value: LiteralValue::Number(42.0),
                    loc: input.get_loc("42"..":"),
                }),
                value: Expression::Literal(Literal {
                    value: LiteralValue::String("'bar'".to_string()),
                    loc: input.get_loc("'bar'".."}"),
                }),
                kind: "init".to_string(),
                loc: input.get_loc("42".."}")
            }
        ],
        loc: input.get_loc(..)
    });
}

#[test]
fn it_ignores_whitespaces() {
    check_expression(" this ", |input| Expression::ThisExpression {
        loc: input.get_loc("t".." ")
    });
}
