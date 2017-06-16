
use std::ops;
use std::str;
use std::error;
use std::fmt;

use text;

pub(crate) fn identifier_eq(left: &str, right: &str) -> bool {
    use std::ascii::{ AsciiExt };
    left.is_ascii() && right.is_ascii() && left.eq_ignore_ascii_case(right)
}

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    value: text::Text,
}

impl Identifier {

    pub fn from_static_str(value: &'static str) -> Result<Identifier, IdentifierError> {
        Ok(Identifier {
            value: text::Text::from_static_str(validate(value)?),
        })
    }

    pub(crate) fn to_string(&self) -> String { (*self.value).into() }

    pub(crate) fn is_eq(&self, value: &str) -> bool {
        identifier_eq(self, value)
    }

    pub(crate) fn into_value(self) -> text::Value {
        let deferred = self.value.into_deferred();
        text::Value::from_deferred(deferred)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdentifierError {
    Empty,
    NotAscii {
        identifier: String,
    },
    Whitespace {
        identifier: String,
        whitespace: char,
    },
    Forbidden {
        identifier: String,
        forbidden: char,
    },
}

impl fmt::Display for IdentifierError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IdentifierError::Empty =>
                write!(fmt, "Value is empty"),
            IdentifierError::NotAscii { ref identifier } =>
                write!(fmt, "Identifier '{}' contains non-ASCII characters", identifier),
            IdentifierError::Forbidden { forbidden, ref identifier } =>
                write!(fmt, "Identifier '{}' contains forbidden character '{}'",
                    identifier,
                    forbidden.escape_default(),
                ),
            IdentifierError::Whitespace { whitespace, ref identifier } =>
                write!(fmt, "Identifier '{}' contains whitespace character '{}'",
                    identifier,
                    whitespace.escape_default(),
                ),
        }
    }
}

impl error::Error for IdentifierError {

    fn description(&self) -> &str { "Invalid identifier value" }
}

pub(crate) fn validate(value: &str) -> Result<&str, IdentifierError> {
    use std::ascii::{ AsciiExt };

    const FORBIDDEN: &[char] = &['=', '"', '/', '>', '<', '&', ',', ';'];

    if value.is_empty() {
        return Err(IdentifierError::Empty);
    }
    if !value.is_ascii() {
        return Err(IdentifierError::NotAscii {
            identifier: value.into(),
        });
    }
    if let Some(whitespace) = value.chars().find(|c| c.is_whitespace()) {
        return Err(IdentifierError::Whitespace {
            identifier: value.into(),
            whitespace,
        });
    }
    if let Some(forbidden) = value.matches(FORBIDDEN).next() {
        return Err(IdentifierError::Forbidden {
            identifier: value.into(),
            forbidden: forbidden.chars().nth(0).expect("found char"),
        });
    }

    Ok(value)
}

impl str::FromStr for Identifier {

    type Err = IdentifierError;

    fn from_str(value: &str) -> Result<Self, Self::Err> {
        validate(value)
            .map(|value| Identifier { value: value.into() })
    }
}

impl ops::Deref for Identifier {

    type Target = str;

    fn deref(&self) -> &str { &self.value }
}

impl PartialEq for Identifier {

    fn eq(&self, other: &Identifier) -> bool {
        identifier_eq(&self.value, &other.value)
    }
}

impl fmt::Display for Identifier {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, fmt)
    }
}

pub trait IntoIdentifier {

    fn into_identifier(self) -> Result<Identifier, IdentifierError>;
}

impl IntoIdentifier for String {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { self.parse() }
}

impl IntoIdentifier for &'static str {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> {
        Identifier::from_static_str(self)
    }
}

impl IntoIdentifier for Result<Identifier, IdentifierError> {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { self }
}

impl IntoIdentifier for Identifier {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { Ok(self) }
}

