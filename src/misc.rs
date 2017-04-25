
fn var_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

named!(pub identifier_name< &str, &str >, do_parse!(
    return_error!(es_error!(InvalidVarName), peek!(none_of!("0123456789"))) >>
    id: take_while1_s!(var_name_char) >>
    (id)
));


