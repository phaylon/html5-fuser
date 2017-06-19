
use std::rc;
use std::cell;

use text;
use modifier;
use event;
use builder;
use transform;

use super::{ Shared };

type SharedPeek<S> = Shared<modifier::Peek<modifier::Expand<S>>>;

pub struct SelectContent<S, B>
where
    S: event::ElementStream,
    B: builder::BuildOnce<CurrentContent<S>>,
{
    state: modifier::State<SelectContentState<S, B>>,
}

impl<S, B> SelectContent<S, B>
where
    S: event::ElementStream,
    B: builder::BuildOnce<CurrentContent<S>>,
{
    pub(crate) fn new(source: S, builder: B) -> SelectContent<S, B> {
        let source = modifier::Expand::new(source);
        SelectContent {
            state: modifier::State::new(SelectContentState::Open { source, builder }),
        }
    }
}

impl<S, B> event::ElementStream for SelectContent<S, B>
where
    S: event::ElementStream,
    B: builder::BuildOnce<CurrentContent<S>>,
{}

impl<S, B> event::Stream for SelectContent<S, B>
where
    S: event::ElementStream,
    B: builder::BuildOnce<CurrentContent<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            SelectContentState::Open { mut source, builder } => {
                let event = match source.next_event_skip_noop()? {
                    Some(event) => event,
                    None => return Err(event::StreamError::expected_open(None)),
                };
                let tag = match event {
                    event::Event::OpeningTag { ref tag, .. } => tag.clone(),
                    event::Event::VoidTag { tag, attributes } => return Ok(Some((
                        event::void(tag, attributes),
                        None,
                    ))),
                    other => return Err(event::StreamError::expected_open(Some(other))),
                };
                let source = rc::Rc::new(cell::RefCell::new(
                    modifier::Peek::new(source),
                ));
                let stream = transform::build_once(
                    CurrentContent::new(source.clone(), tag.clone()),
                    builder,
                );
                Ok(Some((event, Some(SelectContentState::Content { source, stream, tag }))))
            },
            SelectContentState::Content { source, mut stream, tag } =>
                match stream.next_event()? {
                    None => Ok(Some((
                        event::noop(),
                        Some(SelectContentState::Close { source, tag }),
                    ))),
                    Some(event) => Ok(Some((
                        event,
                        Some(SelectContentState::Content { source, stream, tag }),
                    ))),
                },
            SelectContentState::Close { mut source, tag } => {
                let event = match source.next_event_skip_noop()? {
                    Some(event::Event::ClosingTag { tag: closed_tag }) =>
                        if closed_tag == tag {
                            event::close(closed_tag)
                        } else {
                            return Err(
                                event::StreamError::unexpected_close(closed_tag.to_string())
                            );
                        },
                    other => return Err(event::StreamError::expected_close(other)),
                };
                Ok(Some((event, None)))
            },
        })
    }
}

enum SelectContentState<S, B>
where
    S: event::ElementStream,
    B: builder::BuildOnce<CurrentContent<S>>,
{
    Open {
        source: modifier::Expand<S>,
        builder: B,
    },
    Content {
        source: SharedPeek<S>,
        stream: B::Stream,
        tag: text::Identifier,
    },
    Close {
        source: SharedPeek<S>,
        tag: text::Identifier,
    },
}

pub struct CurrentContent<S> {
    state: modifier::State<CurrentContentState<S>>,
}

impl<S> CurrentContent<S> where S: event::Stream {

    fn new(stream: SharedPeek<S>, tag: text::Identifier) -> CurrentContent<S> {
        CurrentContent {
            state: modifier::State::new(CurrentContentState {
                tag,
                stream,
                level: modifier::Level::new(),
            }),
        }
    }
}

fn has_reached_end<S: event::Stream>(
    stream: &mut SharedPeek<S>,
    level: &modifier::Level,
    tag: &text::Identifier,
) -> Result<bool, event::StreamError> {
    if !level.is_top_level() {
        return Ok(false);
    }
    match stream.borrow_mut().peek(|event| event.is_closing_tag_for(tag))? {
        Some(value) => Ok(value),
        None => Err(event::StreamError::missing_close(tag.to_string())),
    }
}

impl<S> event::Stream for CurrentContent<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|CurrentContentState { tag, mut stream, level }| {
            if has_reached_end(&mut stream, &level, &tag)? {
                return Ok(None);
            }
            let event = match stream.next_event()? {
                Some(event) => event,
                None => return Err(event::StreamError::missing_close(tag.to_string())),
            };
            let level = level.adjust(&event)
                .map_err(|_| event::StreamError::missing_close(tag.to_string()))?;
            Ok(Some((event, Some(CurrentContentState { tag, level, stream }))))
        })
    }
}

struct CurrentContentState<S> {
    tag: text::Identifier,
    stream: SharedPeek<S>,
    level: modifier::Level,
}
