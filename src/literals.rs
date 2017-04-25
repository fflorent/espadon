use std::str::{self, FromStr};
use nom::{self, IResult, digit};

/// Literal value
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Number(f32),
    String(String),
    Boolean(bool)
}

/// Literal Expression
/// https://github.com/estree/estree/blob/master/es5.md#literal
#[derive(Debug, PartialEq)]
pub struct Literal {
    pub value: LiteralValue
}

/// null literal value parser
#[doc(hidden)]
named!(null_literal_value< &str, LiteralValue >, do_parse!(
    tag!("null") >>
    (LiteralValue::Null)
));

/// boolean literal value parser
#[doc(hidden)]
named!(boolean_literal_value< &str, LiteralValue >, alt!(
    tag!("true")  => { |_| LiteralValue::Boolean(true)  } |
    tag!("false") => { |_| LiteralValue::Boolean(false) }
));

// Largely inspired from nom (thanks to geal)
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L23-L48
// FIXME support hexadecimal, octal and binary expressions too

/// number literal value parser
#[doc(hidden)]
named!(number_literal_value< &str, LiteralValue >, map_res!(
    recognize!(
        alt_complete!(
            delimited!(digit, tag!("."), opt!(complete!(digit))) |
            delimited!(opt!(digit), tag!("."), digit)            |
            digit
        )
    ),
    |value_as_str: &str| {
        FromStr::from_str(value_as_str).and_then(|value| Ok(LiteralValue::Number(value)))
    }
));

/// string literal value parser
#[doc(hidden)]
named!(string_literal_value< &str, LiteralValue >, do_parse!(
    string: call!(eat_string) >>
    (LiteralValue::String(string.to_string()))
));

/// generic literal value parser
#[doc(hidden)]
named!(literal_value< &str, LiteralValue >, alt_complete!(
    number_literal_value |
    null_literal_value |
    boolean_literal_value |
    string_literal_value
));

/// Literal parser
#[doc(hidden)]
named!(pub literal< &str, Literal >, map!(
    literal_value,
    |value| (Literal {
        value: value
    })
));

// Helpers

/// Returns a whole string (with its delimiters), escaping the backslashes
fn eat_string(input: &str) -> IResult< &str, &str > {
    if input.len() == 0 {
        return IResult::Incomplete(nom::Needed::Unknown);
    }
    let mut chars = input.char_indices();

    let separator = match chars.nth(0) {
        sep @ Some((_, '\'')) | sep @ Some((_, '"')) => sep.unwrap().1,
        // FIXME meaningfull error codes
        Some(_) | None => return nom::IResult::Error(error_position!(es_error!(InvalidString), input))
    };

    let mut escaped = false;
    for (idx, item) in chars {
        if escaped {
            escaped = false;
        } else {
            match item {
                c if c == separator => return IResult::Done(&input[idx+1..], &input[0..idx+1]),
                '\\' => escaped = true,
                '\n' => return IResult::Incomplete(nom::Needed::Unknown),
                _ => ()
            }
        }
    }

    return IResult::Error(error_position!(es_error!(InvalidString), input));
}

