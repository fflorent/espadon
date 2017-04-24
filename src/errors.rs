pub enum ErrorCode {
    INVALID_STRING,
    INVALID_VAR_NAME
}

#[macro_export]
macro_rules! es_error (
    ($errName: ident) => { ::nom::ErrorKind::Custom($crate::errors::ErrorCode::$errName as u32) }
);
