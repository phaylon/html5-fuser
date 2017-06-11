
use std::rc;
use std::cell;
use std::fmt;
use std::error;

use text;
use template;

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

    pub(crate) fn is_self_contained_tag(&self) -> bool {
        match *self {
            Event::SelfClosedTag { .. } | Event::VoidTag { .. } => true,
            _ => false,
        }
    }

    pub(crate) fn is_opening_tag_for(&self, tag_name: &text::Identifier) -> bool {
        self.opening_tag_name()
            .map(|name| *name == *tag_name)
            .unwrap_or(false)
    }

    pub(crate) fn is_closing_tag_for_str(&self, tag_name: &str) -> bool {
        match *self {
            Event::ClosingTag { ref tag, .. } => text::identifier_eq(tag, tag_name),
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

    pub(crate) fn is_closing_tag(&self) -> bool {
        self.closing_tag_name().is_some()
    }

    pub(crate) fn is_opening_tag(&self) -> bool {
        self.opening_tag_name().is_some()
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attribute {
    name: text::Identifier,
    value: Option<text::Value>,
}

impl Attribute {

    pub(crate) fn new(name: text::Identifier, value: Option<text::Value>) -> Attribute {
        Attribute {
            name: name,
            value: value,
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

#[derive(Debug)]
pub enum StreamError {
    Malformed {
        error: MalformedReason,
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
}

impl fmt::Display for StreamError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StreamError::Malformed { ref error } =>
                write!(fmt, "Malformed event stream: {}", error),
            StreamError::Input { .. } =>
                write!(fmt, "Input error for inserted content stream"),
            StreamError::File { .. } =>
                write!(fmt, "Input error for inserted file content stream"),
            StreamError::Identifier { .. } =>
                write!(fmt, "Invalid attribute identifier"),
        }
    }
}

impl error::Error for StreamError {

    fn description(&self) -> &str { "Stream error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            StreamError::Malformed { .. } => None,
            StreamError::Input { ref error } => Some(error),
            StreamError::File { ref error } => Some(error),
            StreamError::Identifier { ref error } => Some(error),
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

    pub(crate) fn missing_close(tag_name: String) -> StreamError {
        StreamError::Malformed {
            error: MalformedReason::MissingClosingTag { tag_name },
        }
    }

    pub(crate) fn expected_open(found_event: Option<Event>) -> StreamError {
        StreamError::Malformed {
            error: MalformedReason::ExpectedOpeningTag { found_event },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MalformedReason {
    MissingClosingTag {
        tag_name: String,
    },
    ExpectedOpeningTag {
        found_event: Option<Event>,
    },
}

impl fmt::Display for MalformedReason {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MalformedReason::MissingClosingTag { ref tag_name } =>
                write!(fmt, "Missing closing tag for '{}'", tag_name),
            MalformedReason::ExpectedOpeningTag { ref found_event } =>
                write!(fmt, "Expected opening tag, found {}",
                     found_event.as_ref()
                        .map(Event::description)
                        .unwrap_or("end of input"),
                ),
        }
    }
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

impl<T> Stream for Box<T> where T: Stream {

    fn next_event(&mut self) -> StreamResult {
        (**self).next_event()
    }
}

impl<T> Stream for rc::Rc<cell::RefCell<T>> where T: Stream {

    fn next_event(&mut self) -> StreamResult {
        self.borrow_mut().next_event()
    }
}

pub trait ElementStream: Stream {}

impl<T> ElementStream for Box<T> where T: ElementStream {}

pub trait IntoStream {

    type Stream: Stream;

    fn into_stream(self) -> Self::Stream;
}

impl<S> IntoStream for S where S: Stream {

    type Stream = S;

    fn into_stream(self) -> S { self }
}

impl<S, E> IntoStream for Result<S, E>
where
    S: IntoStream,
    E: Into<StreamError>,
{
    type Stream = TryStream<S::Stream>;

    fn into_stream(self) -> Self::Stream {
        TryStream {
            stream: self
                .map(IntoStream::into_stream)
                .map_err(Into::into)
                .map_err(Some),
        }
    }
}

#[derive(Debug)]
pub struct TryStream<S> {
    stream: Result<S, Option<StreamError>>,
}

impl<S> Stream for TryStream<S> where S: Stream {

    fn next_event(&mut self) -> StreamResult {
        match self.stream {
            Ok(ref mut stream) => stream.next_event(),
            Err(ref mut error) => match error.take() {
                Some(error) => Err(error),
                None => Ok(None),
            },
        }
    }
}
