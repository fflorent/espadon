use super::*;
use nom::{IResult};

mod literals;
mod expressions;
mod statements;

fn it_parses_multiple_statements() {
    assert_eq!(program("var test1; 42;"), IResult::Done("", Program {
        body: vec![
            Statement::VariableDeclaration(VariableDeclaration {
                declarations: vec![VariableDeclarator {
                    id: "test".to_string(),
                    init: None
                }],
                kind: "var".to_string()
            }),
            Statement::Expression(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0)
            }))
        ]
    }));
}

#[test]
fn it_parses_statements_with_whitespaces_around() {
    assert_eq!(program(" var test;"), program("var test;"));
    assert_eq!(program(" var   test   ;"), program("var test;"));
    assert_eq!(program(" var   test   ;   ").to_result(), program("var test;").to_result());
}

