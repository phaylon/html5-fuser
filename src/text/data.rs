
use std::str;
use std::fmt;

use tendril;

use text;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    text: text::EncodedText,
}

impl fmt::Display for Data {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.text, fmt)
    }
}

impl Data {

    pub(crate) fn from_encoded_tendril(value: tendril::StrTendril) -> Data {
        Data {
            text: text::EncodedText::from_encoded_tendril(value),
        }
    }

    pub(crate) fn from_raw(value: &str) -> Data {
        Data {
            text: text::EncodedText::from_raw(value),
        }
    }

    pub fn from_unencoded_str(value: &str) -> Data {
        Data {
            text: text::EncodedText::from_unencoded_str(value),
        }
    }

    pub fn from_unencoded_static_str(value: &'static str) -> Data {
        Data {
            text: text::EncodedText::from_unencoded_static_str(value),
        }
    }
}

