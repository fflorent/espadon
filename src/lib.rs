#[macro_use]
extern crate nom;

#[macro_use] pub mod errors;
#[cfg(test)] mod tests;
mod misc;
mod literals;
mod expressions;
mod statements;

pub use self::literals::Literal;
pub use self::expressions::Expression;
use literals::LiteralValue;

pub use self::statements::{VariableDeclarator,
    VariableDeclaration,
    ForInitializer,
    Statement,
    statement,
    statements_set
};

#[derive(Debug, PartialEq)]
pub struct Program {
    pub body: Vec<Statement>
}

named!(pub program< &str, Program >, do_parse!(
    body: call!(statements_set) >>
    (Program {
        body: body
    })
));

