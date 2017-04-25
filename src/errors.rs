pub enum ErrorCode {
    InvalidString,
    InvalidVarName
}

#[macro_export]
macro_rules! es_error (
    ($errName: ident) => { ::nom::ErrorKind::Custom($crate::errors::ErrorCode::$errName as u32) }
);
