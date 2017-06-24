
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

    test_group!(add_class:
        "add to existing" => transform_test!(
            "<a class=\"foo bar\">23</a>",
            "<a class=\"foo bar baz\">23</a>",
            |html| html.select("a", |html| html.add_class("baz")),
        ),
        "add to none" => transform_test!(
            "<a>23</a>",
            "<a class=\"baz\">23</a>",
            |html| html.select("a", |html| html.add_class("baz")),
        ),
        "ensure element stream" => transform_test!(
            "<a>23</a>",
            "<a class=\"baz qux\">23</a>",
            |html| html.select("a", |html| html.add_class("baz").add_class("qux")),
        ),
    );

    test_group!(remove_class:
        "remove one of many" => transform_test!(
            "<a class=\"foo  bar baz\" id=\"xyz\">23</a>",
            "<a class=\"foo baz\" id=\"xyz\">23</a>",
            |html| html.select("a", |html| html.remove_class("bar")),
        ),
        "remove last class" => transform_test!(
            "<a class=\"bar\" id=\"xyz\">23</a>",
            "<a id=\"xyz\">23</a>",
            |html| html.select("a", |html| html.remove_class("bar")),
        ),
        "ensure element stream" => transform_test!(
            "<a class=\"bar baz\" id=\"xyz\">23</a>",
            "<a id=\"xyz\">23</a>",
            |html| html.select("a", |html| html.remove_class("bar").remove_class("baz")),
        ),
    );

    test_group!(set_id:
        "set with existing id" => transform_test!(
            "<link id=\"foo\" class=\"bar\">",
            "<link class=\"bar\" id=\"qux\">",
            |html| html.select("link", |html| html.set_id("qux")),
        ),
        "set without existing id" => transform_test!(
            "<link class=\"bar\">",
            "<link class=\"bar\" id=\"qux\">",
            |html| html.select("link", |html| html.set_id("qux")),
        ),
        "ensure element stream" => transform_test!(
            "<link class=\"bar\">",
            "<link class=\"bar\" id=\"qux\">",
            |html| html.select("link", |html| html.set_id("first").set_id("qux")),
        ),
    );

    test_group!(remove_id:
        "remove existing" => transform_test!(
            "<link id=\"foo\" class=\"bar\">",
            "<link class=\"bar\">",
            |html| html.select("link", |html| html.remove_id()),
        ),
        "ensure element stream" => transform_test!(
            "<link id=\"foo\" class=\"bar\">",
            "<link class=\"bar\">",
            |html| html.select("link", |html| html.remove_id().remove_id()),
        ),
    );

    test_group!(remove_attribute:
        "remove existing" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html.select("a", |html| html.remove_attribute("foo")),
        ),
        "ensure element stream" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html.select("a", |html| html.remove_attribute("foo").remove_attribute("foo")),
        ),
    );

    test_group!(set_attribute:
        "replace existing attribute" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.set_attribute("foo", Some("newval"))),
        ),
        "add new attribute" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.set_attribute("foo", Some("newval"))),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html
                .set_attribute("foo", Some("newval"))
                .set_attribute("foo", Some("newval"))
            ),
        ),
    );

    test_group!(set_attribute_value:
        "replace existing attribute" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.set_attribute_value("foo", "newval")),
        ),
        "add new attribute" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.set_attribute_value("foo", "newval")),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html
                .set_attribute_value("foo", "newval")
                .set_attribute_value("foo", "newval")
            ),
        ),
    );

    test_group!(set_empty_attribute:
        "replace existing with value" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html.select("a", |html| html.set_empty_attribute("foo")),
        ),
        "add new attribute" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html.select("a", |html| html.set_empty_attribute("foo")),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html.select("a", |html| html
                .set_empty_attribute("foo")
                .set_empty_attribute("foo")
            ),
        ),
    );

    test_group!(replace_attribute_value_if_exists:
        "replace existing" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html
                .replace_attribute_value_if_exists("foo", "newval")
            ),
        ),
        "replace multiple" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\" foo=\"false\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html
                .replace_attribute_value_if_exists("foo", "newval")
            ),
        ),
        "none to replace" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html.select("a", |html| html
                .replace_attribute_value_if_exists("foo", "newval")
            ),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\">23</a>",
            |html| html.select("a", |html| html
                .replace_attribute_value_if_exists("foo", "newval")
                .replace_attribute_value_if_exists("foo", "newval")
            ),
        ),
    );

    test_group!(add_to_attribute:
        "add to existing" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar:newval\" baz=\"qux\">23</a>",
            |html| html.select("a", |html| html.add_to_attribute("foo", "newval", ":")),
        ),
        "add new" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.add_to_attribute("foo", "newval", ":")),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval:newval2\">23</a>",
            |html| html.select("a", |html| html
                .add_to_attribute("foo", "newval", ":")
                .add_to_attribute("foo", "newval2", ":")
            ),
        ),
    );

    test_group!(add_attribute:
        "add to existing" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar\" baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.add_attribute("foo", "newval")),
        ),
        "add new" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\">23</a>",
            |html| html.select("a", |html| html.add_attribute("foo", "newval")),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo=\"newval\" foo=\"newval2\">23</a>",
            |html| html.select("a", |html| html
                .add_attribute("foo", "newval")
                .add_attribute("foo", "newval2")
            ),
        ),
    );

    test_group!(add_empty_attribute:
        "add to existing with value" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\">23</a>",
            "<a foo=\"bar\" baz=\"qux\" foo>23</a>",
            |html| html.select("a", |html| html.add_empty_attribute("foo")),
        ),
        "add new" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo>23</a>",
            |html| html.select("a", |html| html.add_empty_attribute("foo")),
        ),
        "ensure element stream" => transform_test!(
            "<a baz=\"qux\">23</a>",
            "<a baz=\"qux\" foo foo>23</a>",
            |html| html.select("a", |html| html
                .add_empty_attribute("foo")
                .add_empty_attribute("foo")
            ),
        ),
    );

    test_group!(void_elements:
        "remove attribute" => transform_test!(
            "<link foo=\"bar\" baz=\"qux\">",
            "<link baz=\"qux\">",
            |html| html.select("link", |html| html.remove_attribute("foo")),
        ),
    );

    test_group!(self_closed_elements:
        "remove attribute" => transform_test!(
            "<a foo=\"bar\" baz=\"qux\" />",
            "<a baz=\"qux\" />",
            |html| html.select("a", |html| html.remove_attribute("foo")),
        ),
    );
}
