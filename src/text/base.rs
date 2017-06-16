
use std::ops;
use std::str;
use std::fmt;

use tendril;
use text;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Text {
    value: text::Deferred<tendril::StrTendril>,
}

impl Text {

    pub(crate) fn into_deferred(self) -> text::Deferred<tendril::StrTendril> {
        self.value
    }

    pub(crate) fn from_static_str(value: &'static str) -> Text {
        Text {
            value: text::Deferred::StaticStr(value),
        }
    }
}

impl<'s> From<&'s str> for Text {

    fn from(value: &'s str) -> Text {
        Text {
            value: text::Deferred::Actual(value.into()),
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

