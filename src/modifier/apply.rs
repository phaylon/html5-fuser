
use event;
use template;
use modifier;

#[derive(Debug)]
pub struct ApplyEither<S1, S2> {
    stream: OneOf<S1, S2>
}

impl<S1, S2> ApplyEither<S1, S2> {

    pub(crate) fn first(stream: S1) -> ApplyEither<S1, S2> {
        ApplyEither {
            stream: OneOf::First(stream),
        }
    }

    pub(crate) fn second(stream: S2) -> ApplyEither<S1, S2> {
        ApplyEither {
            stream: OneOf::Second(stream),
        }
    }
}

impl<S1, S2> event::ElementStream for ApplyEither<S1, S2>
where
    S1: event::ElementStream,
    S2: event::ElementStream,
{}

impl<S1, S2> event::Stream for ApplyEither<S1, S2>
where
    S1: event::Stream,
    S2: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        match self.stream {
            OneOf::First(ref mut stream) => stream.next_event(),
            OneOf::Second(ref mut stream) => stream.next_event(),
        }
    }
}

#[derive(Debug)]
enum OneOf<S1, S2> {
    First(S1),
    Second(S2),
}

#[derive(Debug)]
pub struct TemplateApply<S, B> {
    state: modifier::State<TemplateState<S, B>>,
}

impl<S, B> TemplateApply<S, B> {

    pub(crate) fn new(stream: S, builder: B) -> TemplateApply<S, B> {
        TemplateApply {
            state: modifier::State::new(TemplateState::Collect { stream, builder }),
        }
    }
}

impl<S, B> event::Stream for TemplateApply<S, B>
where
    S: event::Stream,
    B: FnOnce(template::Template) -> Result<template::Template, event::StreamError>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            TemplateState::Collect { stream, builder } => {
                let template = template::Template::from_stream(stream)?;
                let applied = builder(template)?;
                let stream = applied.to_stream();
                Ok(Some((event::noop(), Some(TemplateState::Emit { stream }))))
            },
            TemplateState::Emit { stream } =>
                modifier::passthrough(stream, |stream| TemplateState::Emit { stream }),
        })
    }
}

#[derive(Debug)]
enum TemplateState<S, B> {
    Collect {
        stream: S,
        builder: B,
    },
    Emit {
        stream: template::TemplateStream,
    },
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn apply_as_template() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b>foo</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply_as_template(|template| template
                        .transform(|html| html
                            .select(::select::Tag::from_str("b").unwrap(), |html| html
                                .replace_contents("foo")
                            )
                        )
                    )
                )
        );
    }

    #[test]
    fn apply() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply(|html| html
                        .remove_contents()
                    )
                )
        );
    }

    #[test]
    fn apply_if_else() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b>99</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply_if_else(true,
                        |html| html.replace_contents("99"),
                        |html| html.replace_contents("33"),
                    )
                )
        );
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b>33</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply_if_else(false,
                        |html| html.replace_contents("99"),
                        |html| html.replace_contents("33"),
                    )
                )
        );
    }

    #[test]
    fn apply_if() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b>99</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply_if(true, |html| html
                        .replace_contents("99")
                    )
                )
        );
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b>23</b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .apply_if(false, |html| html
                        .replace_contents("99")
                    )
                )
        );
    }
}
