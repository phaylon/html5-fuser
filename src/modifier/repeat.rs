
use event;
use modifier;
use transform;
use template;

pub type Source = template::TemplateStream;

#[derive(Debug)]
pub struct Repeat<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    iter: I,
    state: modifier::State<State<S, X>>,
    builder: B,
}

#[derive(Debug)]
enum State<S, X> {
    Collect {
        stream: S
    },
    Next {
        template: template::Template,
    },
    Emit {
        template: template::Template,
        stream: X,
    },
}

impl<S, I, B, X> Repeat<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(iter: I, stream: S, builder: B) -> Repeat<S, I, B, X> {
        Repeat {
            iter,
            builder,
            state: modifier::State::new(State::Collect { stream }),
        }
    }
}

impl<S, I, B, X> event::Stream for Repeat<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        let Repeat { ref mut iter, ref mut builder, ref mut state } = *self;
        state.step(|state| match state {
            State::Collect { stream } => Ok(Some((
                event::noop(),
                Some(State::Next { template: template::Template::from_stream(stream)? }),
            ))),
            State::Next { template } => match iter.next() {
                None => Ok(None),
                Some(item) => Ok(Some((
                    event::noop(),
                    {
                        let stream = template.to_stream();
                        Some(State::Emit {
                            template,
                            stream: transform::Api::unpack(builder(
                                transform::Api::pack(stream),
                                item,
                            )),
                        })
                    },
                ))),
            },
            State::Emit { mut stream, template } => match stream.next_event_skip_noop()? {
                None => Ok(Some((event::noop(), Some(State::Next { template })))),
                Some(event) => Ok(Some((event, Some(State::Emit { stream, template })))),
            },
        })
    }
}

#[derive(Debug)]
pub struct RepeatContents<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    stream: modifier::select::ContentActorOnce<S, BuildRepeat<I, B>>,
}

impl<S, I, B, X> RepeatContents<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(iter: I, stream: S, builder: B) -> RepeatContents<S, I, B, X> {
        RepeatContents {
            stream: modifier::select::ContentActorOnce::new(
                stream,
                BuildRepeat { iter, builder },
            ),
        }
    }
}

impl<S, I, B, X> event::Stream for RepeatContents<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<S, I, B, X> event::ElementStream for RepeatContents<S, I, B, X>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{}

#[derive(Debug)]
struct BuildRepeat<I, B> {
    iter: I,
    builder: B,
}

impl<S, I, B, X> transform::BuildOnce<S> for BuildRepeat<I, B>
where
    S: event::Stream,
    I: Iterator,
    B: for<'t> FnMut(transform::Api<'t, Source>, I::Item) -> transform::Api<'t, X>,
    X: event::Stream,
{
    type Stream = Repeat<S, I, B, X>;

    fn build_once(self, stream: S) -> Self::Stream {
        Repeat::new(self.iter, stream, self.builder)
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn repeat_contents() {
        test_transform!(
            Default::default(),
            "<a><b>23<c></c>45</b></a>",
            "<a><b>23<c>3</c>4523<c>4</c>45</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .repeat_contents(3..5, |html, n| html
                        .select(::select::Tag::from_str("c").unwrap(), move |html| html
                            .replace_contents(n)
                        )
                    )
                )
        );
    }

    #[test]
    fn repeat() {
        test_transform!(
            Default::default(),
            "<a><b>23<c></c>45</b></a>",
            "<a><b>23<c>3</c>45</b><b>23<c>4</c>45</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .repeat(3..5, |html, n| html
                        .select(::select::Tag::from_str("c").unwrap(), move |html| html
                            .replace_contents(n)
                        )
                    )
                )
        );
    }
}
