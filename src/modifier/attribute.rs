
//! Element attribute transformations.

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
            Some(event::Event(event::EventKind::OpeningTag { tag, attributes })) =>
                Ok(Some((
                    event::open(tag, modify(attributes, data)),
                    Some(State::Emit { stream }),
                ))),
            Some(event::Event(event::EventKind::SelfClosedTag { tag, attributes })) =>
                Ok(Some((event::self_closed(tag, modify(attributes, data)), None))),
            Some(event::Event(event::EventKind::VoidTag { tag, attributes })) =>
                Ok(Some((event::void(tag, modify(attributes, data)), None))),
            other => Err(event::StreamError::expected_open(other)),
        },
        State::Emit { stream } => modifier::passthrough(stream, |stream| State::Emit { stream }),
    }
}

/// Remove all `class` attributes.
#[derive(Debug)]
pub struct RemoveClass<S> {
    state: modifier::State<State<S, text::Identifier>>,
}

impl<S> event::ElementStream for RemoveClass<S> where S: event::ElementStream {}

impl<S> event::Stream for RemoveClass<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, name|
            attributes.remove_class(name)
        ))
    }
}

impl<S> RemoveClass<S> where S: event::ElementStream {

    pub(crate) fn new(stream: S, name: text::Identifier)
    -> RemoveClass<S> {
        RemoveClass {
            state: modifier::State::new(State::Start {
                stream,
                data: name,
            }),
        }
    }
}

/// Remove an attribute.
#[derive(Debug)]
pub struct RemoveAttribute<S> {
    state: modifier::State<State<S, text::Identifier>>,
}

impl<S> event::ElementStream for RemoveAttribute<S> where S: event::ElementStream {}

impl<S> event::Stream for RemoveAttribute<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, name|
            attributes.remove_attribute(name)
        ))
    }
}

impl<S> RemoveAttribute<S> where S: event::ElementStream {

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

/// Add an attribute to an element.
#[derive(Debug)]
pub struct AddAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::ElementStream for AddAttribute<S> where S: event::ElementStream {}

impl<S> event::Stream for AddAttribute<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.add_attribute(name, value)
        ))
    }
}

impl<S> AddAttribute<S> where S: event::ElementStream {

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

/// Replace an attribute with a new value.
#[derive(Debug)]
pub struct ReplaceAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::ElementStream for ReplaceAttribute<S> where S: event::ElementStream {}

impl<S> event::Stream for ReplaceAttribute<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.replace_attribute(name, value)
        ))
    }
}

impl<S> ReplaceAttribute<S> where S: event::ElementStream {

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

/// Add a value to an attribute.
#[derive(Debug)]
pub struct AddToAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, text::Value, text::Value)>>,
}

impl<S> event::ElementStream for AddToAttribute<S> where S: event::ElementStream {}

impl<S> event::Stream for AddToAttribute<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value, separator)|
            attributes.add_to_attribute(name, value, separator)
        ))
    }
}

impl<S> AddToAttribute<S> where S: event::ElementStream {

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

/// Set an attribute value.
#[derive(Debug)]
pub struct SetAttribute<S> {
    state: modifier::State<State<S, (text::Identifier, Option<text::Value>)>>,
}

impl<S> event::ElementStream for SetAttribute<S> where S: event::ElementStream {}

impl<S> event::Stream for SetAttribute<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| attribute_step(state, |attributes, (name, value)|
            attributes.set_attribute(name, value)
        ))
    }
}

impl<S> SetAttribute<S> where S: event::ElementStream {

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
    fn add_class() {
        test_transform!(
            Default::default(),
            "<a class=\"foo bar\">23</a>",
            "<a class=\"foo bar baz\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_class("baz")
                )
        );
        test_transform!(
            Default::default(),
            "<a>23</a>",
            "<a class=\"baz\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .add_class("baz")
                )
        );
    }

    #[test]
    fn remove_class() {
        test_transform!(
            Default::default(),
            "<a class=\"foo  bar baz\" id=\"xyz\">23</a>",
            "<a class=\"foo baz\" id=\"xyz\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .remove_class("bar")
                )
        );
        test_transform!(
            Default::default(),
            "<a class=\"bar\" id=\"xyz\">23</a>",
            "<a id=\"xyz\">23</a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .remove_class("bar")
                )
        );
    }

    #[test]
    fn set_id() {
        test_transform!(
            Default::default(),
            "<link id=\"foo\" class=\"bar\">",
            "<link class=\"bar\" id=\"qux\">",
            |html| html
                .select(::select::Tag::from_str("link").unwrap(), |html| html
                    .set_id("qux")
                )
        );
        test_transform!(
            Default::default(),
            "<link class=\"bar\">",
            "<link class=\"bar\" id=\"qux\">",
            |html| html
                .select(::select::Tag::from_str("link").unwrap(), |html| html
                    .set_id("qux")
                )
        );
    }

    #[test]
    fn remove_id() {
        test_transform!(
            Default::default(),
            "<link id=\"foo\" class=\"bar\">",
            "<link class=\"bar\">",
            |html| html
                .select(::select::Tag::from_str("link").unwrap(), |html| html
                    .remove_id()
                )
        );
    }

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
