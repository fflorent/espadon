use super::*;
use nom::Needed;
use literals::literal;

fn check_literal_value(literal_to_parse: &str, expected_value: LiteralValue) {
    assert_eq!(literal(literal_to_parse), IResult::Done("", Literal {
        value: expected_value
    }));
}

fn check_incomplete_literal(literal_to_parse: &str, needed: Needed) {
    assert_eq!(literal(literal_to_parse), IResult::Incomplete(needed));
}

#[test]
fn it_parses_null() {
    check_literal_value("null", LiteralValue::Null);
}

#[test]
fn it_parses_decimal_number() {
    check_literal_value("42", LiteralValue::Number(42.0));
    check_literal_value("42.42", LiteralValue::Number(42.42));
    check_literal_value(".42", LiteralValue::Number(0.42));
}

// skip
// #[test]
fn it_parses_octal_number() {
    check_literal_value("0o10", LiteralValue::Number(8.0));
}

#[test]
fn it_parses_string() {
    check_literal_value("\"foo\"", LiteralValue::String("\"foo\"".to_string()));
    check_literal_value("'foo'", LiteralValue::String("'foo'".to_string()));
}

#[test]
fn it_parses_non_standard_multi_line_string() {
    let string_to_test = "\"foo$\\\n bar\"";

    check_literal_value(string_to_test,
                        LiteralValue::String(string_to_test.to_string()));
}

#[test]
fn it_parses_strings_with_escaped_quotes() {
    let string_to_test = "\"foo\\\"bar\"";

    check_literal_value(string_to_test,
                        LiteralValue::String(string_to_test.to_string()));
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
