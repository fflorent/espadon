use nom::{self, IResult, digit, Slice};
use misc::{StrSpan, Location};

macro_rules! recognize2 (
    ($i:expr, $submac:ident!( $($args:tt)* )) => ({
        use nom::Offset;
        use nom::Slice;
        match $submac!($i, $($args)*) {
            $crate::IResult::Done(i,_)     => {
                let index = ($i).offset(&i);
                $crate::IResult::Done(i, ($i).slice(..index))
            },
            $crate::IResult::Error(e)      => $crate::IResult::Error(e),
            $crate::IResult::Incomplete(i) => $crate::IResult::Incomplete(i)
        }
    });
    ($i:expr, $f:expr) => (
        recognize2!($i, call!($f))
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
    Boolean(bool),
    Regexp { pattern: String, flags: Vec<char> },
}

/// [A literal expression]
/// (https://github.com/estree/estree/blob/master/es5.md#literal)
#[derive(Debug, PartialEq)]
pub struct Literal<'a> {
    pub value: LiteralValue,
    pub loc: Location<'a>
}

/// null literal value parser
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-NullLiteral
named!(null_literal_value< StrSpan, LiteralValue >, do_parse!(
    tag!("null") >>
    (LiteralValue::Null)
));

/// boolean literal value parser
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-BooleanLiteral
named!(boolean_literal_value< StrSpan, LiteralValue >, alt!(
    tag!("true")  => { |_| LiteralValue::Boolean(true)  } |
    tag!("false") => { |_| LiteralValue::Boolean(false) }
));

/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-SignedInteger
named!(signed_integer< StrSpan, (Option<char>, StrSpan) >, pair!(
    opt!(one_of!("-+")),
    digit
));

// Adapted from Geal's JSON parser
// https://github.com/Geal/nom/blob/66128e5ccf316f60fdd55a7ae8d266f42955b00c/benches/json.rs#L23-L48
/// Parses decimal floats
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-DecimalLiteral
named!(decimal_float< StrSpan, f64 >, map_res!(
    recognize2!(
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
    |raw_value: StrSpan| {
        raw_value.fragment.parse::<f64>()
    }
));

/// Parses octal integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-OctalIntegerLiteral
named!(octal_integer< StrSpan, f64 >, preceded!(
    alt!(
        tag_no_case!("0o") |
        tag!("0")
    ),
    fold_many1!(one_of!("01234567"), 0.0, digit_accumulator_callback(8))
));

/// Parses binary integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-BinaryIntegerLiteral
named!(binary_integer< StrSpan, f64 >, preceded!(
    tag_no_case!("0b"),
    fold_many1!(one_of!("01"), 0.0, digit_accumulator_callback(2))
));

/// Parses hexadecimal integers
/// https://www.ecma-international.org/ecma-262/7.0/index.html#prod-HexIntegerLiteral
named!(hexadecimal_integer< StrSpan, f64 >, preceded!(
    tag_no_case!("0x"),
    fold_many1!(one_of!("0123456789abcdefABCDEF"), 0.0, digit_accumulator_callback(16))
));

/// Number literal value parser, whether that's in a decimal,
/// an octal, an hexadecimal or a binary base
named!(number_literal_value< StrSpan, LiteralValue >, map!(
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
named!(string_literal_value< StrSpan, LiteralValue >, do_parse!(
    string: call!(eat_string) >>
    (LiteralValue::String(string.to_string()))
));
named!(regexp_char< StrSpan, String >, alt!(
    none_of!("\\/") => { |c: char| c.to_string() } |
    preceded!(char!('\\'), take!(1)) => { |c: StrSpan|
        if c.to_string() == "\\" { c.to_string() } else { "\\".to_string() + &c.to_string() }
    }
));


/// regexp literal value parser
named!(regexp_literal_value< StrSpan, LiteralValue >, do_parse!(
    body: delimited!(char!('/'), many1!(regexp_char), char!('/')) >>
    flags: many0!(one_of!("abcdefghijklmnopqrstuvwxyz")) >> // FIXME: be closer to the spec
    (LiteralValue::Regexp {
        pattern: body.join(""),
        flags: flags,
    })
));

/// generic literal value parser
named!(literal_value< StrSpan, LiteralValue >, alt_complete!(
    number_literal_value |
    null_literal_value |
    boolean_literal_value |
    string_literal_value |
    regexp_literal_value
));

/// Literal parser
named!(pub literal< StrSpan, Literal >, es_parse!({
        value: literal_value
    } => (Literal {
        value: value
    })
));

// ==================================================================
// ============================= HELPERS ============================
// ==================================================================

/// Returns a whole string (with its delimiters), escaping the backslashes
fn eat_string(located_span: StrSpan) -> IResult< StrSpan, String > {
    let string = located_span.fragment;
    let error = nom::IResult::Error(error_position!(es_error!(InvalidString), string));
    if string.len() == 0 {
        return IResult::Incomplete(nom::Needed::Unknown);
    }
    let mut chars = string.char_indices();

    let separator = match chars.nth(0) {
        Some((_, sep)) if (sep == '"' || sep == '\'') => sep,
        // FIXME meaningfull error codes
        Some(_) | None => return error
    };

    let mut escaped = false;
    let mut unescaped_string = String::new();

    while let Some((idx, item)) = chars.next() {
        if escaped {
            match item {
                'b' => unescaped_string.push(0x08 as char),
                't' => unescaped_string.push(0x09 as char),
                'n' => unescaped_string.push(0x0a as char),
                'v' => unescaped_string.push(0x0b as char),
                'f' => unescaped_string.push(0x0c as char),
                'r' => unescaped_string.push(0x0d as char),
                '"' => unescaped_string.push(0x22 as char),
                '\'' => unescaped_string.push(0x27 as char),
                '\\' => unescaped_string.push(0x5c as char),
                'x' => {
                    let digits: String = match (chars.next(), chars.next()) {
                        (Some((_, c0)), Some((_, c1))) => vec![c0, c1].iter().collect(),
                        (_, None) => return IResult::Incomplete(nom::Needed::Unknown),
                        (None, Some(_)) => panic!("First call to .next() returned None, but second one did not."),
                    };
                    let c = match u8::from_str_radix(&digits, 16) {
                        Ok(u) => u as char,
                        // TODO: better error
                        _ => return error,
                    };
                    unescaped_string.push(c);
                },
                'u' => {
                    // https://www.ecma-international.org/ecma-262/7.0/index.html#prod-UnicodeEscapeSequence
                    let digits: String = match chars.next() {
                        Some((_, c0)) if c0 == '{' => chars.by_ref().take_while(|&(_, c)| c != '}').map(|(_, c)| c).collect(),
                        Some((_, c0)) => {
                            let c1 = chars.next(); let c2 = chars.next();
                            match chars.next() {
                                Some((_, c3)) => vec![c0, c1.unwrap().1, c2.unwrap().1, c3].iter().collect(), // These unwraps can't panic
                                None => return IResult::Incomplete(nom::Needed::Unknown),
                            }
                        },
                        None => return error,
                    };
                    let c = match u32::from_str_radix(&digits, 16) {
                        Ok(u) if u <= 1114111 => {
                            match ::std::char::from_u32(u) {
                                Some(c) => c,
                                None => return error,
                            }
                        },
                        // TODO: better error
                        _ => return error,
                    };
                    unescaped_string.push(c);
                },
                _ => unescaped_string.push(item),
            };
            escaped = false;
        } else {
            match item {
                c if c == separator => return IResult::Done(located_span.slice(idx+1..), unescaped_string),
                '\\' => escaped = true,
                '\n' => return IResult::Incomplete(nom::Needed::Unknown),
                _ => unescaped_string.push(item),
            }
        }
    }

    return IResult::Error(error_position!(es_error!(InvalidString), string));
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

