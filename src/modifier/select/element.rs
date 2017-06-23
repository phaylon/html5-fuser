
use event;
use text;
use modifier;

/// A stream representing a single element.
#[derive(Debug)]
pub struct SingleElement<S> {
    state: modifier::State<SingleElementState<S>>,
}

impl<S> SingleElement<S> {

    pub(crate) fn new(stream: S, event: event::Event) -> SingleElement<S> {
        SingleElement {
            state: modifier::State::new(SingleElementState::Start { event, stream }),
        }
    }
}

impl<S> event::ElementStream for SingleElement<S> where S: event::Stream {}

impl<S> event::Stream for SingleElement<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            SingleElementState::Start { event, stream } => match event {
                event::Event(event::EventKind::OpeningTag { tag, attributes }) => Ok(Some((
                    event::open(tag.clone(), attributes),
                    Some(SingleElementState::EmitUntilClose { tag, stream, level: 0 }),
                ))),
                event::Event(event::EventKind::SelfClosedTag { tag, attributes }) =>
                    Ok(Some((event::self_closed(tag, attributes), None))),
                event::Event(event::EventKind::VoidTag { tag, attributes }) =>
                    Ok(Some((event::void(tag, attributes), None))),
                other => Err(event::StreamError::expected_open(Some(other))),
            },
            SingleElementState::EmitUntilClose { tag, mut stream, level } => {
                let event = match stream.next_event()? {
                    Some(event) => event,
                    None => return Ok(None),
                };
                if event.is_opening_tag_for(&tag) {
                    Ok(Some((event, Some(SingleElementState::EmitUntilClose {
                        tag,
                        stream,
                        level: level + 1,
                    }))))
                } else if event.is_closing_tag_for(&tag) {
                    if level == 0 {
                        Ok(Some((event, None)))
                    } else {
                        Ok(Some((event, Some(SingleElementState::EmitUntilClose {
                            tag,
                            stream,
                            level: level - 1,
                        }))))
                    }
                } else {
                    Ok(Some((event, Some(SingleElementState::EmitUntilClose {
                        tag, stream, level,
                    }))))
                }
            },
        })
    }
}

#[derive(Debug)]
enum SingleElementState<S> {
    Start {
        event: event::Event,
        stream: S,
    },
    EmitUntilClose {
        tag: text::Identifier,
        stream: S,
        level: usize,
    },
}

