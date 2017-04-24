use super::*;

fn check_expression(expression_to_parse: &str, expected_expression: Expression) {
    assert_eq!(expression(expression_to_parse), IResult::Done("", expected_expression));
}

#[test]
fn it_parses_literal_expressions() {
    check_expression("true", Expression::Literal(Literal {
        value: LiteralValue::Boolean(true)
    }));
}

#[test]
fn it_parses_this_expressions() {
    check_expression("this", Expression::ThisExpression);
}

#[test]
fn it_parses_identifiers() {
    check_expression("undefined", Expression::Identifier {
        name: "undefined".to_string()
    });
}

#[test]
fn it_parses_assignment_expressions() {
    let assignment_operators = vec![
        "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", ">>>=",
        "|=", "^=", "&="
    ];

    for assignment_operator in assignment_operators {
        let expression_to_parse = format!("a {} 42", assignment_operator);

        let expected = Expression::Assignment {
            left: Box::new(Expression::Identifier {
                name: "a".to_string()
            }),
            operator: assignment_operator.to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0)
            }))
        };

        assert_eq!(expression(&expression_to_parse),
                   IResult::Done("", expected),
                   "should parse using operator \"{}\"", assignment_operator);
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

        let expected = Expression::Binary {
            left: Box::new(Expression::Identifier {
                name: "a".to_string()
            }),
            operator: binary_operator.to_string(),
            right: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0)
            }))
        };

        assert_eq!(expression(&expression_to_parse),
                   IResult::Done("", expected),
                   "should parse for {}", binary_operator);
    }
}

