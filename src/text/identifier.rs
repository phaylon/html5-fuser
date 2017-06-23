
use std::ops;
use std::str;
use std::error;
use std::fmt;

use text;

pub(crate) fn identifier_eq(left: &str, right: &str) -> bool {
    use std::ascii::{ AsciiExt };
    left.is_ascii() && right.is_ascii() && left.eq_ignore_ascii_case(right)
}

/// An encapsulated, validated identifier.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::text::{ Identifier };
///
/// // Using std::str::FromStr
/// use std::str::{ FromStr };
/// let identifier = Identifier::from_str("foo")?;
/// let identifier: Identifier = "foo".parse()?;
///
/// // Allowing for static str optimization
/// let identifier = Identifier::from_static_str("foo")?;
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
#[derive(Debug, Clone, Eq)]
pub struct Identifier {
    value: text::Text,
}

impl Identifier {

    /// Constructor allowing static str optimization.
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

/// Details about identifier invalidity.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IdentifierError {
    /// An identifier cannot be zero-length.
    Empty,
    /// The value contained non-ASCII characters.
    NotAscii,
    /// The value contained whitespace.
    Whitespace {
        /// The detected whitespace character.
        whitespace: char,
    },
    /// The value contained a prohibited ASCII character.
    Forbidden {
        /// The detected forbidden character.
        forbidden: char,
    },
}

impl fmt::Display for IdentifierError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IdentifierError::Empty =>
                write!(fmt, "Value is empty"),
            IdentifierError::NotAscii =>
                write!(fmt, "Identifier contains non-ASCII characters"),
            IdentifierError::Forbidden { forbidden } =>
                write!(fmt, "Identifier contains forbidden character '{}'",
                    forbidden.escape_default(),
                ),
            IdentifierError::Whitespace { whitespace } =>
                write!(fmt, "Identifier contains whitespace character '{}'",
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
        return Err(IdentifierError::NotAscii);
    }
    if let Some(whitespace) = value.chars().find(|c| c.is_whitespace()) {
        return Err(IdentifierError::Whitespace { whitespace });
    }
    if let Some(forbidden) = value.matches(FORBIDDEN).next() {
        return Err(IdentifierError::Forbidden {
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

/// Allow conversion into an identifier.
pub trait IntoIdentifier {

    /// Try to convert the value into an identifier.
    fn into_identifier(self) -> Result<Identifier, IdentifierError>;
}

impl IntoIdentifier for &'static str {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> {
        Identifier::from_static_str(self)
    }
}

impl IntoIdentifier for Result<Identifier, IdentifierError> {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { self }
}

impl<'a> IntoIdentifier for &'a Identifier {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { Ok(self.clone()) }
}

impl IntoIdentifier for Identifier {

    fn into_identifier(self) -> Result<Identifier, IdentifierError> { Ok(self) }
}

