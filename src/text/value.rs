
use std::str;
use std::fmt;

use tendril;

use text;

/// An encoded attribute value.
///
/// This value can also be used as content.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::text::{ Value };
///
/// let value = Value::from_unencoded_str("username");
///
/// let template = Template::from_str(r#"
///     <span class="field-name">field-name</span>
///     <input>
/// "#, ParseOptions::default())?;
///
/// let output = format!("{}", template.transform(|html| html
///     .select("input", |html| html
///         .set_attribute_value("name", &value)
///     )
///     .select(".field-name", |html| html
///         .replace_contents(&value)
///     )
/// )?);
///
/// assert!(output.contains(r#"<span class="field-name">username</span>"#));
/// assert!(output.contains(r#"<input name="username">"#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Value {
    text: text::EncodedText,
}

impl fmt::Display for Value {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.text, fmt)
    }
}

impl Value {

    pub(crate) fn new() -> Value {
        Value {
            text: text::EncodedText::new(),
        }
    }

    pub(crate) fn into_encoded_text(self) -> text::EncodedText { self.text }

    pub(crate) fn from_deferred(deferred: text::Deferred<tendril::StrTendril>) -> Value {
        Value {
            text: text::EncodedText::from_deferred(deferred),
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.text.is_empty()
    }

    pub(crate) fn push_encoded_str(&mut self, value: &str) {
        self.text.push_encoded_str(value)
    }

    pub(crate) fn from_raw(value: &str) -> Value {
        Value {
            text: text::EncodedText::from_raw(value),
        }
    }

    /// Construct by encoding the provided value.
    pub fn from_unencoded_str<T>(value: T) -> Value where T: AsRef<str> {
        Value {
            text: text::EncodedText::from_unencoded_str(value.as_ref()),
        }
    }

    /// Construct by providing the provided static value.
    ///
    /// With this method a copy of the actual data is only required when encoding is
    /// necessary.
    pub fn from_unencoded_static_str(value: &'static str) -> Value {
        Value {
            text: text::EncodedText::from_unencoded_static_str(value),
        }
    }

    /// Gives an encoded string slice.
    pub fn as_encoded_ref(&self) -> text::EncodedStr {
        self.text.as_encoded_ref()
    }

    pub(crate) fn join(self, other: Value) -> Value {
        Value {
            text: self.text.join(other.text),
        }
    }
}

/// Allow conversion into a `Value`.
pub trait IntoValue {

    /// Convert into a `Value`.
    fn into_value(self) -> Value;
}

impl IntoValue for Value {

    fn into_value(self) -> Value { self }
}

impl<'a> IntoValue for &'a Value {

    fn into_value(self) -> Value { self.clone() }
}

impl IntoValue for &'static str {

    fn into_value(self) -> Value {
        Value::from_unencoded_static_str(self)
    }
}

macro_rules! impl_value_octal {
    ($name:ty) => {
        impl IntoValue for $name {
            fn into_value(self) -> Value {
                use std::fmt::{ Write };
                let mut tendril = tendril::StrTendril::new();
                write!(tendril, "{}", self).expect("writing octal to value");
                Value {
                    text: text::EncodedText::from_encoded_tendril(tendril),
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

