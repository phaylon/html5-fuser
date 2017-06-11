
use event;
use text;

pub mod select;
pub mod remove;
pub mod prepend;
pub mod replace;
pub mod append;
pub mod repeat;
pub mod apply;
pub mod attribute;

fn passthrough<S, T, F>(mut stream: S, cont_state: F) -> StateResult<T>
where
    F: FnOnce(S) -> T,
    S: event::Stream,
{
    match stream.next_event()? {
        Some(event) => Ok(Some((event, Some(cont_state(stream))))),
        None => Ok(None),
    }
}

#[derive(Debug)]
struct State<T> {
    inner: Option<T>,
}

type StateResult<T> = Result<Option<(event::Event, Option<T>)>, event::StreamError>;

impl<T> State<T> {

    pub fn new(inner: T) -> State<T> {
        State {
            inner: Some(inner),
        }
    }

    pub fn step<F>(&mut self, modifier: F) -> event::StreamResult
    where F: FnOnce(T) -> StateResult<T>
    {
        let old_state = match self.inner.take() {
            Some(state) => state,
            None => return Ok(None),
        };
        let (result, new_state) = match modifier(old_state)? {
            None => return Ok(None),
            Some(result) => result,
        };
        self.inner = new_state;
        Ok(Some(result))
    }
}

#[derive(Debug)]
enum FallibleState<S> {
    Start {
        result: Result<S, event::StreamError>,
    },
    Emit {
        stream: S,
    }
}

#[derive(Debug)]
pub struct Fallible<S> {
    state: State<FallibleState<S>>,
}

impl<S> Fallible<S> where S: event::Stream {

    pub(crate) fn new(result: Result<S, event::StreamError>) -> Fallible<S> {
        Fallible {
            state: State::new(FallibleState::Start { result }),
        }
    }
}

impl<S> event::ElementStream for Fallible<S> where S: event::ElementStream {}

impl<S> event::Stream for Fallible<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            FallibleState::Start { result } => match result {
                Ok(stream) => Ok(Some((event::noop(), Some(FallibleState::Emit { stream })))),
                Err(error) => Err(error),
            },
            FallibleState::Emit { stream } =>
                passthrough(stream, |stream| FallibleState::Emit { stream }),
        })
    }
}

#[derive(Debug)]
struct Peek<S> {
    stream: S,
    buffered: Option<event::Event>,
}

impl<S> Peek<S> where S: event::Stream {

    pub fn new(stream: S) -> Peek<S> {
        Peek {
            stream,
            buffered: None,
        }
    }

    pub fn take_peeked(&mut self) -> Option<event::Event> {
        self.buffered.take()
    }

    pub fn peek<F, R>(&mut self, peeker: F) -> Result<Option<R>, event::StreamError>
    where
        F: for<'e> FnOnce(&'e event::Event) -> R,
    {
        if self.buffered.is_none() {
            self.buffered = self.stream.next_event_skip_noop()?;
        }
        match self.buffered {
            None => Ok(None),
            Some(ref event) => Ok(Some(peeker(event))),
        }
    }
}

impl<S> event::ElementStream for Peek<S> where S: event::ElementStream {}

impl<S> event::Stream for Peek<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        match self.buffered.take() {
            Some(event) => Ok(Some(event)),
            None => self.stream.next_event_skip_noop(),
        }
    }
}

#[derive(Debug)]
struct Combine<S1, S2> {
    state: State<CombineState<S1, S2>>,
}

impl<S1, S2> Combine<S1, S2> {

    pub fn new(stream1: S1, stream2: S2) -> Combine<S1, S2> {
        Combine {
            state: State::new(CombineState::First(stream1, stream2)),
        }
    }
}

impl<S1, S2> event::Stream for Combine<S1, S2>
where
    S1: event::Stream,
    S2: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            CombineState::First(mut stream, other) => match stream.next_event_skip_noop()? {
                Some(event) => Ok(Some((event, Some(CombineState::First(stream, other))))),
                None => Ok(Some((event::noop(), Some(CombineState::Second(other))))),
            },
            CombineState::Second(mut stream) => match stream.next_event_skip_noop()? {
                Some(event) => Ok(Some((event, Some(CombineState::Second(stream))))),
                None => Ok(None),
            },
        })
    }
}

#[derive(Debug)]
enum CombineState<S1, S2> {
    First(S1, S2),
    Second(S2),
}

#[derive(Debug)]
struct Expand<S> {
    state: State<ExpandState<S>>,
}

impl<S> Expand<S> {

    pub fn new(stream: S) -> Expand<S> {
        Expand {
            state: State::new(ExpandState::Start { stream: stream }),
        }
    }
}

impl<S> event::ElementStream for Expand<S> where S: event::Stream {}

impl<S> event::Stream for Expand<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            ExpandState::Start { mut stream } => match stream.next_event_skip_noop()? {
                Some(event::Event::OpeningTag { tag, attributes }) =>
                    Ok(Some((
                        event::open(tag, attributes),
                        Some(ExpandState::EmitStream { stream }),
                    ))),
                Some(event::Event::SelfClosedTag { tag, attributes }) =>
                    Ok(Some((
                        event::open(tag.clone(), attributes),
                        Some(ExpandState::EmitClosingTag { tag }),
                    ))),
                Some(event::Event::VoidTag { tag, attributes }) =>
                    Ok(Some((event::void(tag, attributes), None))),
                other => Err(event::StreamError::expected_open(other)),
            },
            ExpandState::EmitStream { stream } =>
                passthrough(stream, |stream| ExpandState::EmitStream { stream }),
            ExpandState::EmitClosingTag { tag } =>
                Ok(Some((event::close(tag), None))),
        })
    }
}

#[derive(Debug)]
enum ExpandState<S> {
    Start {
        stream: S,
    },
    EmitStream {
        stream: S,
    },
    EmitClosingTag {
        tag: text::Identifier,
    },
}

#[cfg(test)]
mod tests {

    #[test]
    fn peek() {
        use ::event::{ Stream };
        let stream = ::event::IntoStream::into_stream(
            ::Template::from_str("<b></b>", Default::default()).unwrap(),
        );
        let mut peek = super::Peek::new(stream);
        assert_eq!(
            peek.peek(|_| ()).unwrap(),
            Some(())
        );
        assert_eq!(
            peek.take_peeked().unwrap(),
            ::event::Event::OpeningTag {
                tag: "b".parse().unwrap(),
                attributes: ::event::Attributes::new(Vec::new()),
            }
        );
        assert_eq!(
            peek.peek(|_| ()).unwrap(),
            Some(())
        );
        assert_eq!(
            peek.next_event().unwrap().unwrap(),
            ::event::Event::ClosingTag {
                tag: "b".parse().unwrap(),
            }
        );
        assert_eq!(
            peek.peek(|_| ()).unwrap(),
            None
        );
        assert_eq!(
            peek.next_event().unwrap(),
            None
        );
        assert_eq!(
            peek.take_peeked(),
            None
        );
    }

    #[test]
    fn expand() {
        let stream = ::event::IntoStream::into_stream(
            ::Template::from_str("<b></b>", Default::default()).unwrap(),
        );
        let expand = super::Expand::new(stream);
        let mut events = ::event::test_collect(expand).unwrap();
        assert_eq!(
            events.pop(),
            Some(::event::Event::ClosingTag {
                tag: "b".parse().unwrap(),
            })
        );
        assert_eq!(
            events.pop(),
            Some(::event::Event::OpeningTag {
                tag: "b".parse().unwrap(),
                attributes: ::event::Attributes::new(Vec::new()),
            })
        );
        assert_eq!(
            events.pop(),
            None
        );
    }
}

