#[macro_use]
extern crate nom;

#[macro_use] pub mod errors;
#[cfg(test)] mod tests;
mod misc;
mod literals;
mod expressions;
mod statements;

pub use self::literals::{Literal, LiteralValue};
pub use self::expressions::Expression;

pub use self::statements::{VariableDeclarator,
    VariableDeclaration,
    ForInitializer,
    Statement
};
use self::statements::statement_list;

pub use nom::IResult;

#[derive(Debug, PartialEq)]
/// [A program]
/// (https://github.com/estree/estree/blob/master/es5.md#programs)
///
/// Returned by the `parse` function.
pub struct Program {
    pub body: Vec<Statement>
}

named_attr!(#[doc = r#"
The entry point of the library. Call that function in order to
get a parsed `Program`

```ignore
assert_eq!(parse("var test1; 42;"), IResult::Done("", Program {
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
```
"#], pub parse< &str, Program >, do_parse!(
    body: call!(statement_list) >>
    (Program {
        body: body
    })
));
