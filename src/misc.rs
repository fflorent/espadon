use nom_locate::LocatedSpan;

fn var_name_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '$'
}

pub type StrSpan<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq)]
pub struct Location<'a> {
    pub start: StrSpan<'a>,
    pub end: StrSpan<'a>
}

named!(pub identifier_name< StrSpan, StrSpan >, do_parse!(
    return_error!(es_error!(InvalidVarName), peek!(none_of!("0123456789"))) >>
    id: take_while1_s!(var_name_char) >>
    (id)
));

macro_rules! position_result {
    ($start: expr, $end:expr, $ret:path {$($ret_attrs:tt)*}) => ({
        $ret {
            loc: Location {
                start: $start,
                end: $end,
            },
            $($ret_attrs)*
        }
    });
}

#[macro_export]
macro_rules! es_parse {
    ($i:expr, {$($rest:tt)*} => ($ret:path {$($ret_attrs:tt)*})) => ({
        do_parse!($i,
            start: position!() >>
            $($rest)* >>
            end: position!() >>
            (position_result!(start, end, $ret {$($ret_attrs)*})))
    });
}
