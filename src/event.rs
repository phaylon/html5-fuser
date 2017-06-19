
//! Basic functionality for events and event streams.
//!
//! Besides some common error definitions, the only item a library user might have to
//! interact with is the `IntoStream` trait that is used when content is injected into
//! another stream.

use std::rc;
use std::cell;
use std::fmt;
use std::error;
use std::str;

use text;
use template;
use select;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Event {
    Noop,
    Doctype {
        content: text::EncodedText,
    },
    Comment {
        content: text::EncodedText,
    },
    Data {
        content: text::Data,
    },
    RawData {
        content: text::EncodedText,
    },
    OpeningTag {
        tag: text::Identifier,
        attributes: Attributes,
    },
    ClosingTag {
        tag: text::Identifier,
    },
    SelfClosedTag {
        tag: text::Identifier,
        attributes: Attributes,
    },
    VoidTag {
        tag: text::Identifier,
        attributes: Attributes,
    },
}

impl Event {

    pub(crate) fn description(&self) -> &'static str {
        match *self {
            Event::Noop => "noop",
            Event::Doctype { .. } => "doctype",
            Event::Comment { .. } => "comment",
            Event::Data { .. } => "content",
            Event::RawData { .. } => "raw content",
            Event::OpeningTag { .. } => "opening tag",
            Event::ClosingTag { .. } => "closing tag",
            Event::SelfClosedTag { .. } => "self-closed tag",
            Event::VoidTag { .. } => "void tag",
        }
    }

    pub(crate) fn is_opening_tag_for(&self, tag_name: &text::Identifier) -> bool {
        self.opening_tag_name()
            .map(|name| *name == *tag_name)
            .unwrap_or(false)
    }

    pub(crate) fn is_closing_tag_for_str(&self, tag_name: &str) -> bool {
        match *self {
            Event::ClosingTag { ref tag, .. } => tag.is_eq(tag_name),
            _ => false,
        }
    }

    pub(crate) fn is_closing_tag_for(&self, tag_name: &text::Identifier) -> bool {
        match *self {
            Event::ClosingTag { ref tag, .. } => *tag == *tag_name,
            _ => false,
        }
    }

    pub(crate) fn closing_tag_name(&self) -> Option<&text::Identifier> {
        match *self {
            Event::ClosingTag { ref tag, .. } => Some(tag),
            _ => None,
        }
    }

    pub(crate) fn opening_tag_name(&self) -> Option<&text::Identifier> {
        match *self {
            Event::OpeningTag { ref tag, .. } => Some(tag),
            _ => None,
        }
    }
}

pub(crate) fn noop() -> Event { Event::Noop }

pub(crate) fn void(tag: text::Identifier, attributes: Attributes) -> Event {
    Event::VoidTag { tag, attributes }
}

pub(crate) fn open(tag: text::Identifier, attributes: Attributes) -> Event {
    Event::OpeningTag { tag, attributes }
}

pub(crate) fn self_closed(tag: text::Identifier, attributes: Attributes) -> Event {
    Event::SelfClosedTag { tag, attributes }
}

pub(crate) fn close(tag: text::Identifier) -> Event {
    Event::ClosingTag { tag }
}

pub(crate) fn data(content: text::Data) -> Event {
    Event::Data { content }
}

pub(crate) fn raw_data(content: text::EncodedText) -> Event {
    Event::RawData { content }
}

pub(crate) fn doctype(content: text::EncodedText) -> Event {
    Event::Doctype { content }
}

pub(crate) fn comment(content: text::EncodedText) -> Event {
    Event::Comment { content }
}

impl fmt::Display for Event {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Event::Noop => Ok(()),
            Event::Doctype { ref content, .. } =>
                write!(fmt, "<!doctype {}>", content),
            Event::Comment { ref content, .. } =>
                write!(fmt, "<!--{}-->", content),
            Event::Data { ref content, .. } =>
                fmt::Display::fmt(content, fmt),
            Event::RawData { ref content, .. } =>
                fmt::Display::fmt(content, fmt),
            Event::SelfClosedTag { ref tag, ref attributes, .. } =>
                write!(fmt, "<{} />", TagDisplay { tag: tag, attributes: attributes }),
            Event::OpeningTag { ref tag, ref attributes, .. }
            | Event::VoidTag { ref tag, ref attributes, .. } =>
                write!(fmt, "<{}>", TagDisplay { tag: tag, attributes: attributes }),
            Event::ClosingTag { ref tag, ..} =>
                write!(fmt, "</{}>", tag),
        }
    }
}

struct TagDisplay<'t, 'a> {
    tag: &'t text::Identifier,
    attributes: &'a Attributes,
}

impl<'t, 'a> fmt::Display for TagDisplay<'t, 'a> {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(self.tag, fmt)?;
        if !self.attributes.items.is_empty() {
            write!(fmt, " ")?;
            fmt::Display::fmt(self.attributes, fmt)?;
        }
        Ok(())
    }
}

/// A sequence of attributes, in order.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attributes {
    items: rc::Rc<Vec<Attribute>>,
}

impl Attributes {

    pub(crate) fn new(attributes: Vec<Attribute>) -> Attributes {
        Attributes {
            items: rc::Rc::new(attributes),
        }
    }

    pub(crate) fn add_to_attribute(
        self,
        name: text::Identifier,
        new_value: text::Value,
        separator: text::Value,
    ) -> Attributes {
        let mut attributes = Vec::new();
        let mut iter = self.items.iter();
        'search: loop {
            for attribute in &mut iter {
                if attribute.name == name {
                    attributes.push(Attribute::new(
                        attribute.name.clone(),
                        Some(match attribute.value.clone() {
                            Some(value) => value.join(separator).join(new_value),
                            None => new_value,
                        }),
                    ));
                    break 'search;
                } else {
                    attributes.push(attribute.clone());
                }
            }
            attributes.push(Attribute::new(name, Some(new_value)));
            return Attributes::new(attributes);
        }
        for attribute in iter {
            attributes.push(attribute.clone());
        }
        Attributes::new(attributes)
    }

    pub(crate) fn replace_attribute(self, name: text::Identifier, value: Option<text::Value>)
    -> Attributes {
        let mut attributes = Vec::new();
        for attribute in self.items.iter() {
            if attribute.name != name {
                attributes.push(attribute.clone());
            }
        }
        if self.items.len() != attributes.len() {
            attributes.push(Attribute { name, value });
        }
        Attributes::new(attributes)
    }

    pub(crate) fn add_attribute(self, name: text::Identifier, value: Option<text::Value>)
    -> Attributes {
        let mut attributes = (*self.items).clone();
        attributes.push(Attribute { name, value });
        Attributes::new(attributes)
    }

    pub(crate) fn remove_attribute(self, name: text::Identifier)
    -> Attributes {
        let mut attributes = Vec::new();
        for attribute in self.items.iter() {
            if attribute.name != name {
                attributes.push(attribute.clone());
            }
        }
        Attributes::new(attributes)
    }

    pub(crate) fn set_attribute(self, name: text::Identifier, value: Option<text::Value>)
    -> Attributes {
        let mut attributes = Vec::new();
        for attribute in self.items.iter() {
            if attribute.name != name {
                attributes.push(attribute.clone());
            }
        }
        attributes.push(Attribute { name, value });
        Attributes::new(attributes)
    }

    pub(crate) fn remove_class(self, name: text::Identifier)
    -> Attributes {
        if self.has_class(&name) {
            let mut attributes = Vec::new();
            for attribute in self.items.iter().cloned() {
                if let Some(attribute) = attribute.remove_class(&name) {
                    attributes.push(attribute);
                }
            }
            Attributes::new(attributes)
        } else {
            self
        }
    }

    pub(crate) fn has_class(&self, name: &str) -> bool {
        for attribute in self.items.iter() {
            if attribute.has_class(name) {
                return true;
            }
        }
        false
    }

    pub(crate) fn has_id(&self, id: &str) -> bool {
        for attribute in self.items.iter() {
            if attribute.name.is_eq("id") {
                if let Some(ref value) = attribute.value {
                    if value.as_encoded_ref().identifier_eq(id) {
                        return true;
                    }
                }
            }
        }
        false
    }
}

impl fmt::Display for Attributes {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let mut first = true;
        for attribute in self.items.iter() {
            if first {
                first = false;
            } else {
                write!(fmt, " ")?;
            }
            fmt::Display::fmt(attribute, fmt)?;
        }
        Ok(())
    }
}

/// A single attribute with an optional value.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    name: text::Identifier,
    value: Option<text::Value>,
}

pub(crate) struct Classes<'s> {
    iter: str::SplitWhitespace<'s>,
}

impl<'s> Iterator for Classes<'s> {

    type Item = &'s str;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            return match self.iter.next() {
                None => None,
                Some(value) =>
                    if !value.is_empty() {
                        Some(value)
                    } else {
                        continue;
                    },
            };
        }
    }
}

impl Attribute {

    pub(crate) fn new(name: text::Identifier, value: Option<text::Value>) -> Attribute {
        Attribute {
            name: name,
            value: value,
        }
    }

    pub(crate) fn classes(&self) -> Option<Classes> {
        if self.name.is_eq("class") {
            return self.value.as_ref().map(|value|
                Classes { iter: value.as_encoded_ref().split_whitespace() }
            );
        }
        None
    }

    pub(crate) fn has_class(&self, name: &str) -> bool {
        if let Some(classes) = self.classes() {
            for class in classes {
                if text::identifier_eq(name, class) {
                    return true;
                }
            }
        }
        false
    }

    pub(crate) fn remove_class(self, name: &str) -> Option<Attribute> {

        fn make_without(attribute: &Attribute, name: &str) -> Option<text::Value> {
            if !attribute.has_class(name) {
                return None;
            }
            let classes = match attribute.classes() {
                None => return None,
                Some(iter) => iter,
            };
            let mut new_classes = text::Value::new();
            for class in classes {
                if !text::identifier_eq(name, class) {
                    if !new_classes.is_empty() {
                        new_classes.push_encoded_str(" ");
                    }
                    new_classes.push_encoded_str(class);
                }
            }
            Some(new_classes)
        }

        match make_without(&self, name) {
            None => Some(self),
            Some(new_classes) =>
                if new_classes.is_empty() {
                    None
                } else {
                    Some(Attribute {
                        name: self.name,
                        value: Some(new_classes),
                    })
                },
        }
    }
}

impl fmt::Display for Attribute {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.name, fmt)?;
        if let Some(ref value) = self.value {
            write!(fmt, "=\"")?;
            fmt::Display::fmt(value, fmt)?;
            write!(fmt, "\"")?;
        }
        Ok(())
    }
}

pub type StreamResult = Result<Option<Event>, StreamError>;

/// Occurs when an internal invariant wasn't upheld.
///
/// This kind of error should usually not happen and is indicative of a logic or API bug.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AssertionError {
    /// The processed stream was malformed and did not produce an expected sequence or
    /// pattern of events.
    Malformed {
        /// More details about why the stream was considered malformed.
        error: MalformedReason,
    },
}

impl fmt::Display for AssertionError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            AssertionError::Malformed { .. } =>
                write!(fmt, "Malformed event stream"),
        }
    }
}

impl error::Error for AssertionError {

    fn description(&self) -> &str { "Assertion error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            AssertionError::Malformed { ref error } => Some(error),
        }
    }
}

#[derive(Debug)]
pub enum StreamError {
    Assertion {
        error: AssertionError,
    },
    Input {
        error: template::InputError,
    },
    File {
        error: template::FileError,
    },
    Identifier {
        error: text::IdentifierError,
    },
    Selector {
        error: select::Error,
    },
    StaticSelector {
        error: select::StaticError,
    },
}

impl fmt::Display for StreamError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StreamError::Assertion { .. } =>
                write!(fmt, "An internal invariant was not upheld"),
            StreamError::Input { .. } =>
                write!(fmt, "Input error for inserted content stream"),
            StreamError::File { .. } =>
                write!(fmt, "Input error for inserted file content stream"),
            StreamError::Identifier { .. } =>
                write!(fmt, "Invalid attribute identifier"),
            StreamError::Selector { .. } =>
                write!(fmt, "Invalid element selector"),
            StreamError::StaticSelector { .. } =>
                write!(fmt, "Invalid static str selector"),
        }
    }
}

impl error::Error for StreamError {

    fn description(&self) -> &str { "Stream error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            StreamError::Assertion { ref error } => Some(error),
            StreamError::Input { ref error } => Some(error),
            StreamError::File { ref error } => Some(error),
            StreamError::Identifier { ref error } => Some(error),
            StreamError::Selector { ref error } => Some(error),
            StreamError::StaticSelector { ref error } => Some(error),
        }
    }
}

impl From<template::InputError> for StreamError {

    fn from(error: template::InputError) -> StreamError {
        StreamError::Input { error }
    }
}

impl From<template::FileError> for StreamError {

    fn from(error: template::FileError) -> StreamError {
        StreamError::File { error }
    }
}

impl From<text::IdentifierError> for StreamError {

    fn from(error: text::IdentifierError) -> StreamError {
        StreamError::Identifier { error }
    }
}

impl StreamError {

    pub(crate) fn unexpected_close(tag_name: String) -> StreamError {
        StreamError::Assertion {
            error: AssertionError::Malformed {
                error: MalformedReason::UnexpectedClosingTag { tag_name },
            },
        }
    }

    pub(crate) fn missing_close(tag_name: String) -> StreamError {
        StreamError::Assertion {
            error: AssertionError::Malformed {
                error: MalformedReason::MissingClosingTag { tag_name },
            },
        }
    }

    pub(crate) fn expected_close(found_event: Option<Event>) -> StreamError {
        StreamError::Assertion {
            error: AssertionError::Malformed {
                error: MalformedReason::ExpectedClosingTag { found_event },
            },
        }
    }

    pub(crate) fn expected_open(found_event: Option<Event>) -> StreamError {
        StreamError::Assertion {
            error: AssertionError::Malformed {
                error: MalformedReason::ExpectedOpeningTag { found_event },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalformedReason {
    UnexpectedClosingTag {
        tag_name: String,
    },
    MissingClosingTag {
        tag_name: String,
    },
    ExpectedOpeningTag {
        found_event: Option<Event>,
    },
    ExpectedClosingTag {
        found_event: Option<Event>,
    },
}

impl fmt::Display for MalformedReason {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MalformedReason::UnexpectedClosingTag { ref tag_name } =>
                write!(fmt, "Unexpected closing tag for '{}'", tag_name),
            MalformedReason::MissingClosingTag { ref tag_name } =>
                write!(fmt, "Missing closing tag for '{}'", tag_name),
            MalformedReason::ExpectedOpeningTag { ref found_event } =>
                write!(fmt, "Expected opening tag, found {}",
                     found_event.as_ref()
                        .map(Event::description)
                        .unwrap_or("end of input"),
                ),
            MalformedReason::ExpectedClosingTag { ref found_event } =>
                write!(fmt, "Expected closing tag, found {}",
                     found_event.as_ref()
                        .map(Event::description)
                        .unwrap_or("end of input"),
                ),
        }
    }
}

impl error::Error for MalformedReason {

    fn description(&self) -> &str { "Stream validity error" }
}

#[cfg(test)]
pub(crate) fn test_collect<S>(mut stream: S) -> Result<Vec<Event>, StreamError>
where S: Stream {
    let mut events = Vec::new();
    while let Some(event) = stream.next_event_skip_noop()? {
        events.push(event);
    }
    Ok(events)
}

pub trait Stream {

    fn next_event(&mut self) -> StreamResult;

    fn next_event_skip_noop(&mut self) -> StreamResult {
        'events: loop {
            return match self.next_event() {
                Ok(Some(Event::Noop)) => continue 'events,
                other => other,
            };
        }
    }
}

impl<T> Stream for Box<T> where T: Stream + ?Sized {

    fn next_event(&mut self) -> StreamResult {
        (**self).next_event()
    }
}

impl<T> ElementStream for Box<T> where T: ElementStream + ?Sized {}

impl<T> Stream for rc::Rc<cell::RefCell<T>> where T: Stream + ?Sized {

    fn next_event(&mut self) -> StreamResult {
        self.borrow_mut().next_event()
    }
}

impl<T> ElementStream for rc::Rc<cell::RefCell<T>> where T: ElementStream + ?Sized {}

pub trait ElementStream: Stream {}

pub trait IntoStream {

    type Stream: Stream;

    fn into_stream(self) -> Self::Stream;
}
