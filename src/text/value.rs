
use std::str;
use std::fmt;

use tendril;

use text;

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

    pub(crate) fn from_raw(value: &str) -> Value {
        Value {
            text: text::EncodedText::from_raw(value),
        }
    }

    pub fn from_unencoded(value: &str) -> Value {
        Value {
            text: text::EncodedText::from_unencoded(value),
        }
    }

    pub fn from_unencoded_static(value: &'static str) -> Value {
        Value {
            text: text::EncodedText::from_unencoded_static(value),
        }
    }

    pub(crate) fn join(self, other: Value) -> Value {
        Value {
            text: self.text.join(other.text),
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
        Value::from_unencoded(&self)
    }
}

impl IntoValue for &'static str {

    fn into_value(self) -> Value {
        Value::from_unencoded_static(self)
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

