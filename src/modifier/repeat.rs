
//! Repeat a stream multiple times.

use event;
use modifier;
use transform;
use template;
use builder;

/// A stored stream representing an element if the parent stream does.
pub type MaybeElementTemplate<D> = modifier::Derived<template::TemplateStream, D>;

/// Repeat a stream for each item of an iterator.
pub struct Repeat<S, I, B>
where
    I: Iterator,
    B: builder::BuildMutMapped<MaybeElementTemplate<S>, I::Item>,
{
    state: modifier::State<State<S, I, B, MaybeElementTemplate<S>>>,
}

impl<S, I, B> Repeat<S, I, B>
where
    I: Iterator,
    B: builder::BuildMutMapped<MaybeElementTemplate<S>, I::Item>,
{
    pub(crate) fn new(stream: S, iter: I, builder: B) -> Repeat<S, I, B> {
        Repeat {
            state: modifier::State::new(State::Collect { stream, iter, builder }),
        }
    }
}

impl<S, I, B> event::Stream for Repeat<S, I, B>
where
    S: event::Stream,
    I: Iterator,
    B: builder::BuildMutMapped<MaybeElementTemplate<S>, I::Item>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| state.step(modifier::Derived::new))
    }
}

enum State<S, I, B, SB>
where
    I: Iterator,
    B: builder::BuildMutMapped<SB, I::Item>,
{
    Collect {
        stream: S,
        builder: B,
        iter: I,
    },
    Next {
        template: template::Template,
        builder: B,
        iter: I,
    },
    Emit {
        template: template::Template,
        stream: B::Stream,
        builder: B,
        iter: I,
    },
}

impl<S, I, B, SB> State<S, I, B, SB>
where
    I: Iterator,
    B: builder::BuildMutMapped<SB, I::Item>,
    S: event::Stream,
    SB: event::Stream,
{
    fn step<F>(self, stepper: F) -> modifier::StateResult<State<S, I, B, SB>>
    where
        F: FnOnce(template::TemplateStream) -> SB,
    {
        use event::{ Stream };
        match self {
            State::Collect { stream, builder, iter } => {
                let template = template::Template::from_stream(stream)?;
                Ok(Some((event::noop(), Some(State::Next { template, builder, iter }))))
            },
            State::Next { template, mut builder, mut iter } => {
                let item = match iter.next() {
                    Some(value) => value,
                    None => return Ok(None),
                };
                let stream = stepper(template.to_stream());
                let stream = transform::build_mut_mapped(stream, &mut builder, item);
                Ok(Some((event::noop(), Some(State::Emit { template, stream, builder, iter }))))
            },
            State::Emit { template, builder, iter, mut stream } => match stream.next_event()? {
                Some(event) => Ok(Some((
                    event,
                    Some(State::Emit { template, builder, iter, stream }),
                ))),
                None => Ok(Some((
                    event::noop(),
                    Some(State::Next { template, builder, iter }),
                ))),
            },
        }
    }
}

struct RepeatContentStream<S, I, B>
where
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    state: modifier::State<State<S, I, B, template::TemplateStream>>,
}

impl<S, I, B> event::Stream for RepeatContentStream<S, I, B>
where
    S: event::Stream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| state.step(|stream| stream))
    }
}

struct Builder<I, B> {
    iter: I,
    builder: B,
}

impl<S, I, B> builder::BuildOnce<modifier::select::CurrentContent<S>> for Builder<I, B>
where
    S: event::ElementStream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    type Stream = RepeatContentStream<modifier::select::CurrentContent<S>, I, B>;

    fn build_once(self, stream: transform::Api<modifier::select::CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(RepeatContentStream {
            state: modifier::State::new(State::Collect {
                stream: stream.unpack(),
                iter: self.iter,
                builder: self.builder,
            }),
        })
    }
}

/// Repeat the contents of the current element for each item of an iterator.
pub struct RepeatContent<S, I, B>
where
    S: event::ElementStream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    stream: modifier::select::SelectContent<S, Builder<I, B>>,
}

impl<S, I, B> RepeatContent<S, I, B>
where
    S: event::ElementStream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    pub(crate) fn new(stream: S, iter: I, builder: B) -> RepeatContent<S, I, B> {
        RepeatContent {
            stream: modifier::select::SelectContent::new(stream, Builder { iter, builder }),
        }
    }
}

impl<S, I, B> event::Stream for RepeatContent<S, I, B>
where
    S: event::ElementStream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<S, I, B> event::ElementStream for RepeatContent<S, I, B>
where
    S: event::ElementStream,
    I: Iterator,
    B: builder::BuildMutMapped<template::TemplateStream, I::Item>,
{}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    test_group!(repeat_contents:
        "some integers" => transform_test!(
            "<a><b>23<c></c>45</b></a>",
            "<a><b>23<c>3</c>4523<c>4</c>45</b></a>",
            |html| html.select("b", |html| html.repeat_contents(3..5, |html, n| html
                .select("c", move |html| html.replace_contents(n))
            )),
        ),
        "empty iterator" => transform_test!(
            "<a><b>23<c></c>45</b></a>",
            "<a><b></b></a>",
            |html| html.select("b", |html| html.repeat_contents(0..0, |html, n| html
                .select("c", move |html| html.replace_contents(n))
            )),
        ),
    );

    test_group!(repeat:
        "some integers" => transform_test!(
            "<a><b>23<c></c>45</b></a>",
            "<a><b>23<c>3</c>45</b><b>23<c>4</c>45</b></a>",
            |html| html.select("b", |html| html.repeat(3..5, |html, n| html
                .select("c", move |html| html.replace_contents(n))
            )),
        ),
        "empty iterator" => transform_test!(
            "<a><b>23<c></c>45</b></a>",
            "<a></a>",
            |html| html.select("b", |html| html.repeat(0..0, |html, n| html
                .select("c", move |html| html.replace_contents(n))
            )),
        ),
    );
}
