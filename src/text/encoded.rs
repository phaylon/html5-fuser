
use std::str;
use std::fmt;

use tendril;
use text;

const ENCODE_CHARS: &[char] = &['&', '<', '>', '"'];

fn encode_char(chr: char) -> &'static str {
    match chr {
        '&' => "&amp;",
        '<' => "&lt;",
        '>' => "&gt;",
        '"' => "&quot;",
        c => panic!("unexpected char for encode: '{}'", c.escape_default()),
    }
}

fn encode_str_optional(mut input: &str) -> Option<tendril::StrTendril> {
    if input.find(ENCODE_CHARS).is_none() {
        return None;
    }
    let mut tendril = tendril::StrTendril::new();
    'parts: while !input.is_empty() {
        let len = match input.find(ENCODE_CHARS) {
            Some(pos) => pos,
            None => input.len(),
        };
        tendril.push_slice(&input[..len]);
        input = &input[len..];
        let chr = match input.chars().next() {
            None => break 'parts,
            Some(chr) => chr,
        };
        tendril.push_slice(encode_char(chr));
        input = &input[chr.len_utf8()..];
    }
    Some(tendril)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct EncodedText {
    value: text::Deferred<tendril::StrTendril>,
}

impl EncodedText {

    pub(crate) fn new() -> EncodedText {
        let tendril = tendril::StrTendril::new();
        EncodedText {
            value: text::Deferred::Actual(tendril),
        }
    }

    pub(crate) fn push_encoded_str(&mut self, value: &str) {
        self.value.actual().push_slice(value)
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.value.is_empty()
    }

    pub(crate) fn from_deferred(value: text::Deferred<tendril::StrTendril>) -> EncodedText {
        EncodedText { value }
    }

    pub(crate) fn from_encoded_tendril(tendril: tendril::StrTendril) -> EncodedText {
        EncodedText::from_deferred(text::Deferred::Actual(tendril))
    }

    pub(crate) fn from_unencoded_static_str(value: &'static str) -> EncodedText {
        EncodedText {
            value: encode_str_optional(value)
                .map(text::Deferred::Actual)
                .unwrap_or_else(|| text::Deferred::StaticStr(value)),
        }
    }

    pub(crate) fn join(self, other: EncodedText) -> EncodedText {
        if other.value.len() == 0 {
            return self;
        }
        let mut tendril = self.value.into_actual();
        tendril.push_slice(&other.value);
        EncodedText {
            value: text::Deferred::Actual(tendril),
        }
    }

    pub(crate) fn from_unencoded_str(value: &str) -> EncodedText {
        EncodedText {
            value: text::Deferred::Actual(value.into()),
        }
    }

    pub(crate) fn from_raw(value: &str) -> EncodedText {
        EncodedText {
            value: text::Deferred::Actual(value.into()),
        }
    }

    pub fn as_encoded_ref(&self) -> EncodedStr {
        EncodedStr {
            content: &self.value,
        }
    }
}

impl fmt::Display for EncodedText {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.value, fmt)
    }
}

impl From<text::Text> for EncodedText {

    fn from(text: text::Text) -> EncodedText {
        EncodedText {
            value: encode_str_optional(&text)
                .map(text::Deferred::Actual)
                .unwrap_or_else(|| text.into_deferred()),
        }
    }
}

/// An encoded string slice.
///
/// This is used instead of a `&str` slice to have a slight barrier against accidental
/// double-encoding.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct EncodedStr<'a> {
    content: &'a str,
}

impl<'a> fmt::Display for EncodedStr<'a> {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.content, fmt)
    }
}

impl<'a> EncodedStr<'a> {

    pub(crate) fn split_whitespace(&self) -> str::SplitWhitespace<'a> {
        self.content.split_whitespace()
    }

    pub(crate) fn identifier_eq(&self, value: &str) -> bool {
        text::identifier_eq(self.content, value)
    }
}
