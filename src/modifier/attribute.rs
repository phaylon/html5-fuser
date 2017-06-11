
use text;
use modifier;
use event;

#[derive(Debug)]
enum State<S, D> {
    Start {
        stream: S,
        data: D,
    },
    Emit {
        stream: S,
    },
}

fn attribute_step<S, F, D>(state: State<S, D>, modify: F) -> modifier::StateResult<State<S, D>>
where
    S: event::Stream,
    F: FnOnce(event::Attributes, D) -> event::Attributes,
{
    match state {
        State::Start { mut stream, data } => match stream.next_event_skip_noop()? {
            Some(event::Event::OpeningTag { tag, attributes }) =>
                Ok(Some((
                    event::open(tag, modify(attributes, data)),
                    Some(State::Emit { stream }),
                ))),
            Some(event::Event::SelfClosedTag { tag, attributes }) =>
                Ok(Some((event::self_closed(tag, modify(attributes, data)), None))),
            Some(event::Event::VoidTag { tag, attributes }) =>
                Ok(Some((event::void(tag, modify(attributes, data)), None))),
            other => Err(event::StreamError::expected_open(other)),
        },
        State::Emit { stream } => modifier::passthrough(stream, |stream| State::Emit { stream }),
    }
}

#[derive(Debug)]
pub struct RemoveAttribute<S> {
    state: modifier::State<State<S, text::Identifier>>,
}

impl<S> event::Stream for RemoveAttribute<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, name|
            attributes.remove_attribute(name)
        ))
    }
}

impl<S> RemoveAttribute<S> where S: event::Stream {

    pub(crate) fn new(stream: S, name: text::Identifier)
    -> RemoveAttribute<S> {
        RemoveAttribute {
            state: modifier::State::new(State::Start {
                stream,
                data: name,
            }),
        }
    }
}

#[derive(Debug)]
pub struct AddAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::Stream for AddAttribute<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.add_attribute(name, value)
        ))
    }
}

impl<S> AddAttribute<S> where S: event::Stream {

    pub(crate) fn new(stream: S, name: text::Identifier, value: Option<text::Value>)
    -> AddAttribute<S> {
        AddAttribute {
            state: modifier::State::new(State::Start {
                stream,
                data: (name, value),
            }),
        }
    }
}

#[derive(Debug)]
pub struct ReplaceAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::Stream for ReplaceAttribute<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.replace_attribute(name, value)
        ))
    }
}

impl<S> ReplaceAttribute<S> where S: event::Stream {

    pub(crate) fn new(stream: S, name: text::Identifier, value: Option<text::Value>)
    -> ReplaceAttribute<S> {
        ReplaceAttribute {
            state: modifier::State::new(State::Start {
                stream,
                data: (name, value),
            }),
        }
    }
}

#[derive(Debug)]
pub struct AddToAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, text::Value, text::Value)>>,
}

impl<S> event::Stream for AddToAttribute<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value, separator)|
            attributes.add_to_attribute(name, value, separator)
        ))
    }
}

impl<S> AddToAttribute<S> where S: event::Stream {

    pub(crate) fn new(
        stream: S,
        name: text::Identifier,
        value: text::Value,
        separator: text::Value,
    ) -> AddToAttribute<S> {
        AddToAttribute {
            state: modifier::State::new(State::Start {
                stream,
                data: (name, value, separator),
            }),
        }
    }
}

#[derive(Debug)]
pub struct SetAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::Stream for SetAttribute<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.set_attribute(name, value)
        ))
    }
}

impl<S> SetAttribute<S> where S: event::Stream {

    pub(crate) fn new(stream: S, name: text::Identifier, value: Option<text::Value>)
    -> SetAttribute<S> {
        SetAttribute {
            state: modifier::State::new(State::Start {
                stream,
                data: (name, value),
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn void() {
        test_transform!(
            Default::default(),
            "<link foo=\"bar\" baz=\"qux\">",
            "<link baz=\"qux\">",
            |html| html
                .select(::select::Tag::from_str("link").unwrap(), |html| html
                    .remove_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
    }

    #[test]
    fn self_closed() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\" />",
            "<a baz=\"qux\" />",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .remove_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
    }

    #[test]
    fn remove_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .remove_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
    }

    #[test]
    fn set_empty_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .set_empty_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .set_empty_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
    }

    #[test]
    fn set_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .set_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .set_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
    }

    #[test]
    fn replace_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\" foo=\"false\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
    }

    #[test]
    fn add_to_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar:newval\" baz=\"qux\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_to_attribute(
                        ::text::Identifier::from_str("foo").unwrap(),
                        "newval",
                        ":",
                    )
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_to_attribute(
                        ::text::Identifier::from_str("foo").unwrap(),
                        "newval",
                        ":",
                    )
                )
        );
    }

    #[test]
    fn add_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar\" baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_attribute(::text::Identifier::from_str("foo").unwrap(), "newval")
                )
        );
    }

    #[test]
    fn add_empty_attribute() {
        test_transform!(
            Default::default(),
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar\" baz=\"qux\" foo>23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_empty_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
        test_transform!(
            Default::default(),
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_empty_attribute(::text::Identifier::from_str("foo").unwrap())
                )
        );
    }
}
