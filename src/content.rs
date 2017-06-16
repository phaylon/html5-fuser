
use tendril;

use event;
use text;
use modifier;

impl event::IntoStream for text::Data {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        Stream { data: Some(self) }
    }
}

impl event::IntoStream for String {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        Stream { data: Some(text::Data::from_unencoded_str(&self)) }
    }
}

impl event::IntoStream for &'static str {

    type Stream = Stream;

    fn into_stream(self) -> Self::Stream {
        Stream { data: Some(text::Data::from_unencoded_static_str(self)) }
    }
}

impl<S> event::IntoStream for S where S: event::Stream {

    type Stream = S;

    fn into_stream(self) -> S { self }
}

impl<S, E> event::IntoStream for Result<S, E>
where
    S: event::IntoStream,
    E: Into<event::StreamError>,
{
    type Stream = modifier::Fallible<S::Stream>;

    fn into_stream(self) -> Self::Stream {
        modifier::Fallible::new(
            self.map(event::IntoStream::into_stream)
                .map_err(Into::into)
        )
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
                Stream { data: Some(text::Data::from_encoded_tendril(tendril)) }
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
