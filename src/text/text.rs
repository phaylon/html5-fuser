
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

    pub(crate) fn join(self, other: Text) -> Text {
        if other.len() == 0 {
            return self;
        }
        let mut tendril = self.into_tendril();
        tendril.push_slice(&other.value);
        Text::from_tendril(tendril)
    }

    pub(crate) fn into_tendril(self) -> tendril::StrTendril {
        self.value.into_actual()
    }

    pub(crate) fn from_static(value: &'static str) -> Text {
        Text {
            value: text::Deferred::StaticStr(value),
        }
    }

    pub(crate) fn from_tendril(value: tendril::StrTendril) -> Text {
        Text {
            value: text::Deferred::Actual(value),
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

