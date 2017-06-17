use super::*;
use nom::{IResult, InputLength};
use misc::{Identifier, StrSpan, Location};
use tests::utils::GetLocation;

mod utils;
mod literals;
mod expressions;
mod statements;

#[test]
fn it_parses_multiple_statements() {
    let input = StrSpan::new("var test1; 42;");
    let remaining = StrSpan {
        line: 1,
        offset: input.input_len(),
        fragment: ""
    };

    assert_eq!(parse(input), IResult::Done(remaining, Program {
        body: vec![
            Statement::VariableDeclaration(VariableDeclaration {
                declarations: vec![VariableDeclarator {
                    id: Identifier {
                        name: "test1".to_string(),
                        loc: input.get_loc("test1"..";")
                    },
                    init: None,
                    loc: input.get_loc("test1"..";")
                }],
                kind: "var".to_string(),
                loc: input.get_loc(..";")
            }),
            Statement::Expression(Expression::Literal(Literal {
                value: LiteralValue::Number(42.0),
                loc: input.get_loc("42"..";")
            }))
        ]
    }));
}

#[test]
#[cfg(not(feature = "position"))]
fn it_parses_statements_with_whitespaces_around() {
    assert_eq!(parse(StrSpan::new(" var test;")).to_result(),
               parse(StrSpan::new("var test;")).to_result());
    assert_eq!(parse(StrSpan::new(" var   test   ;")).to_result(),
               parse(StrSpan::new("var test;")).to_result());
    assert_eq!(parse(StrSpan::new(" var   test   ;   ")).to_result(),
               parse(StrSpan::new("var test;")).to_result());
}

