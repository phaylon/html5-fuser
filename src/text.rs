
use std::ops;
use std::str;
use std::error;
use std::fmt;

use tendril;

use content;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Deferred<T> {
    StaticStr(&'static str),
    Actual(T),
}

impl<T> Deferred<T> where T: From<&'static str> {

    pub(crate) fn into_actual(self) -> T {
        match self {
            Deferred::StaticStr(value) => value.into(),
            Deferred::Actual(value) => value,
        }
    }
}

impl<T> fmt::Display for Deferred<T> where T: fmt::Display {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Deferred::StaticStr(value) => fmt::Display::fmt(value, fmt),
            Deferred::Actual(ref value) => fmt::Display::fmt(value, fmt),
        }
    }
}

impl<T> ops::Deref for Deferred<T> where T: ops::Deref<Target=str> {

    type Target = str;

    fn deref(&self) -> &str {
        match *self {
            Deferred::StaticStr(value) => value,
            Deferred::Actual(ref value) => value,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
    value: tendril::StrTendril,
}

impl Text {

    pub(crate) fn from_tendril(value: tendril::StrTendril) -> Text {
        Text { value }
    }
}

impl<'s> From<&'s str> for Text {

    fn from(value: &'s str) -> Text {
        Text {
            value: value.into(),
        }
    }
}

impl ops::Deref for Text {

    type Target = str;

    fn deref(&self) -> &str { &self.value }
}

impl fmt::Display for Text {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, fmt)
    }
}

pub(crate) fn identifier_eq(left: &str, right: &str) -> bool {
    use std::ascii::{ AsciiExt };
    left.is_ascii() && right.is_ascii() && left.eq_ignore_ascii_case(right)
}

#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    value: Deferred<tendril::StrTendril>,
}

impl Identifier {

    pub fn to_string(&self) -> String { (*self.value).into() }
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

fn validate_identifier(value: &str) -> Result<&str, IdentifierError> {
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
        validate_identifier(value)
            .map(|value| Identifier { value: Deferred::Actual(value.into()) })
    }
}

impl ops::Deref for Identifier {

    type Target = str;

    fn deref(&self) -> &str { &self.value }
}

impl PartialEq for Identifier {

    fn eq(&self, other: &Identifier) -> bool {
        let self_ref = &self.value;
        let other_ref = &other.value;
        identifier_eq(self_ref, other_ref)
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
        validate_identifier(self)
            .map(|value| Identifier { value: Deferred::StaticStr(value) })
    }
}

impl IntoIdentifier for Result<Identifier, IdentifierError> {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { self }
}

impl IntoIdentifier for Identifier {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { Ok(self) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    text: Deferred<Text>,
}

impl From<String> for Value {

    fn from(value: String) -> Value {
        Value {
            text: Deferred::Actual(content::encode_str(&value)),
        }
    }
}

impl<'s> From<&'s str> for Value {

    fn from(value: &'s str) -> Value {
        Value {
            text: Deferred::Actual(content::encode_str(&value)),
        }
    }
}

impl Value {

    pub(crate) fn from_text(text: Text) -> Value {
        Value {
            text: Deferred::Actual(text),
        }
    }

    pub(crate) fn into_inner(self) -> Text { self.text.into_actual() }

    pub(crate) fn join(self, other: Value) -> Value {
        let mut text = self.text.into_actual();
        text.value.push_slice(&other.text);
        Value {
            text: Deferred::Actual(text),
        }
    }
}

pub trait IntoValue {

    fn into_value(self) -> Value;
}

impl IntoValue for Value {

    fn into_value(self) -> Value { self }
}

impl IntoValue for String {

    fn into_value(self) -> Value {
        self.into()
    }
}

impl IntoValue for &'static str {

    fn into_value(self) -> Value {
        Value {
            text: match content::encode_str_optional(self) {
                Some(text) => Deferred::Actual(text),
                None => Deferred::StaticStr(self),
            },
        }
    }
}

macro_rules! impl_value_octal {
    ($name:ty) => {
        impl IntoValue for $name {
            fn into_value(self) -> Value {
                use std::fmt::{ Write };
                let mut tendril = tendril::StrTendril::new();
                write!(tendril, "{:o}", self).expect("writing octal to value");
                Value {
                    text: Deferred::Actual(Text::from_tendril(tendril)),
                }
            }
        }
    }
}

impl_value_octal!(usize);
impl_value_octal!(u8);
impl_value_octal!(u16);
impl_value_octal!(u32);
impl_value_octal!(u64);

impl_value_octal!(isize);
impl_value_octal!(i8);
impl_value_octal!(i16);
impl_value_octal!(i32);
impl_value_octal!(i64);

