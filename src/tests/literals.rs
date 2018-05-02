use super::*;
use nom::{Needed, Slice, InputLength};
use literals::literal;

fn check_literal_value(literal_to_parse: &str, expected_value: LiteralValue) {
    check_partial_literal_value(literal_to_parse, expected_value, "");
}

fn check_incomplete_literal(literal_to_parse: &str, needed: Needed) {
    assert_eq!(literal(StrSpan::new(literal_to_parse)),
               IResult::Incomplete(needed));
}

fn check_partial_literal_value(literal_to_parse: &str, expected_value: LiteralValue, remaining_str: &str) {
    let input = StrSpan::new(literal_to_parse);
    let remaining = input.slice((input.input_len() - remaining_str.len())..);
    assert_eq!(remaining.fragment, remaining_str);
    assert_eq!(literal(input), IResult::Done(remaining, Literal {
        value: expected_value,
        loc: Location {
            start: input.slice(0..0),
            end: remaining.slice(0..0)
        }
    }));
}


#[test]
fn it_parses_null() {
    check_literal_value("null", LiteralValue::Null);
}

#[test]
fn it_parses_decimal_numbers() {
    check_literal_value("42", LiteralValue::Number(42.0));
    check_literal_value("42.42", LiteralValue::Number(42.42));
    check_literal_value(".42", LiteralValue::Number(0.42));
}

#[test]
fn it_parses_64_bits_numbers() {
    let max_value = std::f64::MAX;
    check_literal_value(&max_value.to_string(), LiteralValue::Number(max_value));
}

#[test]
fn it_parses_beyond_64_numbers_without_crash() {
    let max_value = std::f64::MAX;
    let mut beyond_max_value = max_value.to_string();
    beyond_max_value.push('0');
    check_literal_value(&beyond_max_value.to_string(), LiteralValue::Number(std::f64::INFINITY));
}

#[test]
fn it_parses_exponent_parts_of_decimal() {
    check_literal_value("42e3", LiteralValue::Number(42000.0));
    check_literal_value("42.e3", LiteralValue::Number(42000.0));
    check_literal_value("42.e+3", LiteralValue::Number(42000.0));
    check_literal_value("42000000.e-3", LiteralValue::Number(42000.0));
    check_literal_value(".42e5", LiteralValue::Number(42000.0));

    // 03 is not considered as octal in exponent part
    check_literal_value("42e03", LiteralValue::Number(42000.0));
}

#[test]
fn it_fails_to_parse_invalid_exponent_parts() {
    check_partial_literal_value("0o42e3", LiteralValue::Number(34.0), "e3");
    check_partial_literal_value("042e3", LiteralValue::Number(34.0), "e3");
    check_partial_literal_value("42e0o3", LiteralValue::Number(42.0), "o3");
}

#[test]
fn it_parses_zero() {
    check_literal_value("0", LiteralValue::Number(0.0));
}

#[test]
fn it_parses_octal_numbers() {
    check_literal_value("0o10", LiteralValue::Number(8.0));
    check_literal_value("0O10", LiteralValue::Number(8.0));
    check_literal_value("010", LiteralValue::Number(8.0));
}

#[test]
fn it_fails_to_parse_invalid_octal_numbers() {
    check_partial_literal_value("0o777787", LiteralValue::Number(4095.0), "87");
    check_partial_literal_value("0777787", LiteralValue::Number(4095.0), "87");
    check_partial_literal_value("0O777787", LiteralValue::Number(4095.0), "87");
    check_partial_literal_value("0o7777a7", LiteralValue::Number(4095.0), "a7");
}

#[test]
fn it_parses_binary_numbers() {
    check_literal_value("0b100", LiteralValue::Number(4.0));
    check_literal_value("0B100", LiteralValue::Number(4.0));
}

#[test]
fn it_fails_to_parse_invalid_binary_numbers() {
    check_partial_literal_value("0b10021", LiteralValue::Number(4.0), "21");
}

#[test]
fn it_parses_hexadecimal_numbers() {
    check_literal_value("0x10", LiteralValue::Number(16.0));
    check_literal_value("0xABCDEF", LiteralValue::Number(11259375.0));
    check_literal_value("0xabcdef", LiteralValue::Number(11259375.0));
    check_literal_value("0Xabcdef", LiteralValue::Number(11259375.0));
}

#[test]
fn it_fails_to_parse_invalid_hexadecimal_numbers() {
    check_partial_literal_value("0x7777g7", LiteralValue::Number(30583.0), "g7");
}

#[test]
fn it_parses_string() {
    check_literal_value("\"foo\"", LiteralValue::String("foo".to_string()));
    check_literal_value("'foo'", LiteralValue::String("foo".to_string()));
}

#[test]
fn it_parses_non_standard_multi_line_string() {
    let string_to_test = "\"foo$\\\n bar\"";

    check_literal_value(string_to_test,
                        LiteralValue::String("foo$\n bar".to_string()));
}

#[test]
fn it_parses_strings_with_escaped_quotes() {
    let string_to_test = "\"foo\\\"bar\"";

    check_literal_value(string_to_test,
                        LiteralValue::String("foo\"bar".to_string()));
}

#[test]
fn it_parses_strings_with_escaped_codepoint() {
    check_literal_value("\"foo\\u0020bar\"",
                        LiteralValue::String("foo bar".to_string()));
    check_literal_value("\"foo\\u{20}bar\"",
                        LiteralValue::String("foo bar".to_string()));
    check_literal_value("\"foo\\u{000000000000020}bar\"",
                        LiteralValue::String("foo bar".to_string()));
}

#[test]
fn it_parses_incomplete_string() {
    check_incomplete_literal("\"foo   \r\n bar\";", Needed::Unknown);
    check_incomplete_literal("\"foo   \n bar\";", Needed::Unknown);
}

#[test]
fn it_parses_boolean() {
    check_literal_value("true", LiteralValue::Boolean(true));
    check_literal_value("false", LiteralValue::Boolean(false));
}
