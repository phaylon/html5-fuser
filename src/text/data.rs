
use std::str;
use std::fmt;

use tendril;

use content;
use text;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    text: text::Text,
}

impl fmt::Display for Data {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.text, fmt)
    }
}

impl From<String> for Data {

    fn from(value: String) -> Data {
        Data {
            text: content::encode_str(&value),
        }
    }
}

impl<'s> From<&'s str> for Data {

    fn from(value: &'s str) -> Data {
        Data {
            text: content::encode_str(&value),
        }
    }
}

impl Data {

    pub(crate) fn from_tendril(value: tendril::StrTendril) -> Data {
        Data {
            text: text::Text::from_tendril(value),
        }
    }

    pub(crate) fn from_raw(value: &str) -> Data {
        Data {
            text: value.into(),
        }
    }

    pub fn from_static(value: &'static str) -> Data {
        Data {
            text: content::encode_str_static(value),
        }
    }
}

