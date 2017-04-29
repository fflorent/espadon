use std::str;
use nom::{self, IResult, digit};

/// Just like opt! except that it supports eof.
macro_rules! opt2 (
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        use ::nom::InputLength;
        match ($i).input_len() {
            0 => ::nom::IResult::Done($i, ::std::option::Option::None),
            _ => opt!($i, $submac!($($args)*))
        }
    });
    ($i:expr, $f:expr) => (
        opt2!($i, call!($f));
    );
);

/// [A literal value]
/// (https://github.com/estree/estree/blob/master/es5.md#literal)
///
/// This has to be used with the `Literal` struct
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Null,
    Number(f64),
    String(String),
    Boolean(bool)
}

/// [A literal expression]
/// (https://github.com/estree/estree/blob/master/es5.md#literal)
#[derive(Debug, PartialEq)]
pub struct Literal {
    pub value: LiteralValue
}

/// null literal value parser
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-NullLiteral
named!(null_literal_value< &str, LiteralValue >, do_parse!(
    tag!("null") >>
    (LiteralValue::Null)
));

/// boolean literal value parser
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-BooleanLiteral
named!(boolean_literal_value< &str, LiteralValue >, alt!(
    tag!("true")  => { |_| LiteralValue::Boolean(true)  } |
    tag!("false") => { |_| LiteralValue::Boolean(false) }
));

/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-SignedInteger
named!(signed_integer< &str, (Option<char>, &str) >, pair!(
    opt!(one_of!("-+")),
    digit
));

// Adapted from Geal's JSON parser
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L23-L48
/// Parses decimal floats
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-DecimalLiteral
named!(decimal_float< &str, f64 >, map_res!(
    recognize!(
        pair!(
            alt_complete!(
                delimited!(digit, tag!("."), opt!(complete!(digit))) |
                delimited!(opt!(digit), tag!("."), digit)            |
                digit
            ),
            opt2!(
                preceded!(
                    tag_no_case!("e"),
                    call!(signed_integer)
                )
            )
        )
    ),
    |value_as_str: &str| {
        value_as_str.parse::<f64>()
    }
));

/// Parses octal integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-OctalIntegerLiteral
named!(octal_integer< &str, f64 >, preceded!(
    alt!(
        tag_no_case!("0o") |
        tag!("0")
    ),
    fold_many1!(one_of!("01234567"), 0.0, digit_accumulator_callback(8))
));

/// Parses binary integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-BinaryIntegerLiteral
named!(binary_integer< &str, f64 >, preceded!(
    tag_no_case!("0b"),
    fold_many1!(one_of!("01"), 0.0, digit_accumulator_callback(2))
));

/// Parses hexadecimal integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-HexIntegerLiteral
named!(hexadecimal_integer< &str, f64 >, preceded!(
    tag_no_case!("0x"),
    fold_many1!(one_of!("0123456789abcdefABCDEF"), 0.0, digit_accumulator_callback(16))
));

/// Number literal value parser, whether that's in a decimal,
/// an octal, an hexadecimal or a binary base
named!(number_literal_value< &str, LiteralValue >, map!(
    alt_complete!(
        octal_integer |
        binary_integer |
        hexadecimal_integer |
        decimal_float
    ), |value| {
        LiteralValue::Number(value)
    }
));

/// string literal value parser
named!(string_literal_value< &str, LiteralValue >, do_parse!(
    string: call!(eat_string) >>
    (LiteralValue::String(string.to_string()))
));

/// generic literal value parser
named!(literal_value< &str, LiteralValue >, alt_complete!(
    number_literal_value |
    null_literal_value |
    boolean_literal_value |
    string_literal_value
));

/// Literal parser
named!(pub literal< &str, Literal >, map!(
    literal_value,
    |value| (Literal {
        value: value
    })
));

// ==================================================================
// ============================= HELPERS ============================
// ==================================================================

/// Returns a whole string (with its delimiters), escaping the backslashes
fn eat_string(input: &str) -> IResult< &str, &str > {
    if input.len() == 0 {
        return IResult::Incomplete(nom::Needed::Unknown);
    }
    let mut chars = input.char_indices();

    let separator = match chars.nth(0) {
        Some((_, sep)) if (sep == '"' || sep == '\'') => sep,
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

type DigitAccumulatorCallback = Fn(f64, char) -> f64;

/// Returns an accumulator callback for bases other than decimal.
fn digit_accumulator_callback(base: u32) -> Box<DigitAccumulatorCallback> {
    Box::new(move |acc: f64, digit_as_char: char| {
        let digit = digit_as_char.to_digit(base)
            .expect("unexpected character encountered") as f64;

        acc * (base as f64) + digit
    })
}

