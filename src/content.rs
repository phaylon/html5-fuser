
use tendril;

use event;
use text;

const ENCODE_CHARS: &[char] = &['&', '<', '>', '"'];

pub(crate) fn encode_char(chr: char) -> &'static str {
    match chr {
        '&' => "&amp;",
        '<' => "&lt;",
        '>' => "&gt;",
        '"' => "&quot;",
        c => panic!("unexpected char for encode: '{}'", c.escape_default()),
    }
}

pub(crate) fn encode_str_optional(mut input: &str) -> Option<text::Text> {
    let mut tendril = tendril::StrTendril::new();
    'parts: while !input.is_empty() {
        let len = match input.find(ENCODE_CHARS) {
            Some(pos) => pos,
            None => return None,
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
    Some(text::Text::from_tendril(tendril))
}

pub(crate) fn encode_str_static(input: &'static str) -> text::Text {
    encode_str_optional(input)
        .unwrap_or_else(|| text::Text::from_static(input))
}

pub(crate) fn encode_str(input: &str) -> text::Text {
    encode_str_optional(input)
        .unwrap_or_else(|| input.into())
}

impl event::IntoStream for text::Data {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        Stream { data: Some(self) }
    }
}

impl event::IntoStream for String {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        event::IntoStream::into_stream(&*self)
    }
}

impl<'s> event::IntoStream for &'s str {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        Stream { data: Some(self.into()) }
    }
}

macro_rules! impl_octal {
    ($name:ty) => {
        impl event::IntoStream for $name {
            type Stream = Stream;
            fn into_stream(self) -> Self::Stream {
                use std::fmt::{ Write };
                let mut tendril = tendril::StrTendril::new();
                write!(tendril, "{:o}", self).expect("writing octal to template");
                Stream { data: Some(text::Data::from_tendril(tendril)) }
            }
        }
    }
}

impl_octal!(usize);
impl_octal!(u8);
impl_octal!(u16);
impl_octal!(u32);
impl_octal!(u64);

impl_octal!(isize);
impl_octal!(i8);
impl_octal!(i16);
impl_octal!(i32);
impl_octal!(i64);

#[derive(Debug)]
pub struct Stream {
    data: Option<text::Data>,
}

impl event::Stream for Stream {

    fn next_event(&mut self) -> event::StreamResult {
        Ok(self.data.take().map(event::data))
    }
}
