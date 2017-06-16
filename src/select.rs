
use std::str;
use std::rc;
use std::fmt;
use std::error;

use text;
use event;

pub trait Selector {

    fn matches(&self, tag: &text::Identifier, attributes: &event::Attributes) -> bool;
}

impl<T> Selector for rc::Rc<T> where T: Selector {
    
    fn matches(&self, tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        (**self).matches(tag, attributes)
    }
}

impl<'a, T> Selector for &'a T where T: Selector {
    
    fn matches(&self, tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        (*self).matches(tag, attributes)
    }
}

pub trait IntoSelector {

    type Error: Into<event::StreamError>;
    type Selector: Selector;

    fn into_selector(self) -> Result<Self::Selector, Self::Error>;
}

impl<T> IntoSelector for T where T: Selector {

    type Error = event::StreamError;
    type Selector = Self;

    fn into_selector(self) -> Result<Self, Self::Error> { Ok(self) }
}

impl<S, E> IntoSelector for Result<S, E>
where
    S: Selector,
    E: Into<event::StreamError>,
{
    type Error = E;
    type Selector = S;

    fn into_selector(self) -> Result<S, E> { self }
}

#[derive(Debug)]
pub enum IdError {
    Identifier {
        error: text::IdentifierError,
    },
}

impl fmt::Display for IdError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            IdError::Identifier { .. } =>
                write!(fmt, "Invalid identifier for id selector"),
        }
    }
}

impl error::Error for IdError {

    fn description(&self) -> &str { "id selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            IdError::Identifier { ref error } => Some(error),
        }
    }
}

impl From<IdError> for event::StreamError {

    fn from(error: IdError) -> event::StreamError {
        match error {
            IdError::Identifier { error } => event::StreamError::Selector {
                error: Error::Id { error },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Id {
    identifier: text::Identifier,
}

impl str::FromStr for Id {

    type Err = IdError;

    fn from_str(value: &str) -> Result<Id, Self::Err> {
        Ok(Id {
            identifier: value.parse().map_err(|error| IdError::Identifier { error })?,
        })
    }
}

impl Selector for Id {

    fn matches(&self, _tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        attributes.has_id(&self.identifier)
    }
}

#[derive(Debug)]
pub enum TagError {
    Identifier {
        error: text::IdentifierError,
    },
}

impl fmt::Display for TagError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TagError::Identifier { .. } =>
                write!(fmt, "Invalid identifier for tag name selector"),
        }
    }
}

impl error::Error for TagError {

    fn description(&self) -> &str { "tag name selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            TagError::Identifier { ref error } => Some(error),
        }
    }
}

impl From<TagError> for event::StreamError {

    fn from(error: TagError) -> event::StreamError {
        match error {
            TagError::Identifier { error } => event::StreamError::Selector {
                error: Error::Tag { error },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    identifier: text::Identifier,
}

impl str::FromStr for Tag {

    type Err = TagError;

    fn from_str(value: &str) -> Result<Tag, Self::Err> {
        Ok(Tag {
            identifier: value.parse().map_err(|error| TagError::Identifier { error })?,
        })
    }
}

impl Selector for Tag {

    fn matches(&self, tag: &text::Identifier, _attributes: &event::Attributes) -> bool {
        *tag == self.identifier
    }
}

#[derive(Debug)]
pub enum ClassError {
    Identifier {
        error: text::IdentifierError,
    },
}

impl fmt::Display for ClassError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassError::Identifier { .. } =>
                write!(fmt, "Invalid identifier for class selector"),
        }
    }
}

impl error::Error for ClassError {

    fn description(&self) -> &str { "class selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            ClassError::Identifier { ref error } => Some(error),
        }
    }
}

impl From<ClassError> for event::StreamError {

    fn from(error: ClassError) -> event::StreamError {
        match error {
            ClassError::Identifier { error } => event::StreamError::Selector {
                error: Error::Class { error },
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Class {
    identifier: text::Identifier,
}

impl str::FromStr for Class {

    type Err = ClassError;

    fn from_str(value: &str) -> Result<Class, Self::Err> {
        Ok(Class {
            identifier: value.parse().map_err(|error| ClassError::Identifier { error })?,
        })
    }
}

impl Selector for Class {

    fn matches(&self, _tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        attributes.has_class(&self.identifier)
    }
}

#[derive(Debug)]
pub enum ClassesError {
    Empty,
    Identifier {
        index: usize,
        error: text::IdentifierError,
    },
}

impl fmt::Display for ClassesError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassesError::Identifier { index, .. } =>
                write!(fmt, "Invalid identifier for class selector at index {}", index),
            ClassesError::Empty =>
                write!(fmt, "List of selected classes is empty"),
        }
    }
}

impl error::Error for ClassesError {

    fn description(&self) -> &str { "classes selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            ClassesError::Identifier { ref error, .. } => Some(error),
            ClassesError::Empty => None,
        }
    }
}

impl From<ClassesError> for event::StreamError {

    fn from(error: ClassesError) -> event::StreamError {
        event::StreamError::Selector {
            error: Error::Classes { error },
        }
    }
}

#[derive(Debug, Clone)]
pub struct Classes {
    identifiers: Vec<text::Identifier>,
}

impl Classes {

    pub fn from_str_iterator<'i, I>(iter: I) -> Result<Classes, ClassesError>
    where I: Iterator<Item=&'i str>
    {
        let mut identifiers = Vec::new();
        for (index, value) in iter.enumerate() {
            identifiers.push(match value.parse() {
                Ok(identifier) => identifier,
                Err(error) => return Err(ClassesError::Identifier { index, error }),
            });
        }
        if identifiers.is_empty() {
            return Err(ClassesError::Empty);
        }
        Ok(Classes { identifiers })
    }
}

impl Selector for Classes {

    fn matches(&self, _tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        for identifier in &self.identifiers {
            if !attributes.has_class(identifier) {
                return false;
            }
        }
        true
    }
}

#[derive(Debug)]
pub enum StaticError {
    Unrestricted,
    Tag {
        error: text::IdentifierError,
    },
    Id {
        error: text::IdentifierError,
    },
    Class {
        index: usize,
        error: text::IdentifierError,
    },
}

impl fmt::Display for StaticError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StaticError::Unrestricted =>
                write!(fmt, "Empty static selector matches everything"),
            StaticError::Id { .. } =>
                write!(fmt, "Invalid identifier for id in static selector"),
            StaticError::Tag { .. } =>
                write!(fmt, "Invalid identifier for tag name in static selector"),
            StaticError::Class { index, .. } =>
                write!(fmt, "Invalid identifier for class (index {}) in static selector",
                    index,
                ),
        }
    }
}

impl error::Error for StaticError {

    fn description(&self) -> &str { "classes selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            StaticError::Unrestricted => None,
            StaticError::Id { ref error }
            | StaticError::Tag { ref error }
            | StaticError::Class { ref error, .. } => Some(error),
        }
    }
}

impl From<StaticError> for event::StreamError {

    fn from(error: StaticError) -> event::StreamError {
        event::StreamError::StaticSelector { error }
    }
}

#[derive(Debug, Clone)]
pub struct Static {
    tag: Option<&'static str>,
    id: Option<&'static str>,
    classes: Option<&'static str>,
}

impl Selector for Static {

    fn matches(&self, tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        if let Some(match_tag) = self.tag {
            if !tag.is_eq(match_tag) {
                return false;
            }
        }
        if let Some(match_id) = self.id {
            if !attributes.has_id(match_id) {
                return false;
            }
        }
        if let Some(classes) = self.classes {
            for class in classes.split('.') {
                if !attributes.has_class(class) {
                    return false;
                }
            }
        }
        true
    }
}

impl IntoSelector for &'static str {

    type Selector = Static;
    type Error = StaticError;

    fn into_selector(self) -> Result<Self::Selector, Self::Error> {

        const MARK_ID: char = '#';
        const MARK_CLASS: char = '.';

        let rest = self;
        let (tag, rest) = {
            let tag_len = match rest.find(&[MARK_ID, MARK_CLASS][..]) {
                Some(len) => len,
                None => rest.len(),
            };
            if tag_len == 0 {
                (None, rest)
            } else {
                let tag = text::validate_identifier(&rest[..tag_len])
                    .map_err(|error| StaticError::Tag { error })?;
                (Some(tag), &rest[tag_len..])
            }
        };
        let (id, rest, expect_class) =
            if let Some(MARK_ID) = rest.chars().next() {
                let rest = &rest[MARK_ID.len_utf8() ..];
                let (id, rest, expect_class) = match rest.find(MARK_CLASS) {
                    Some(len) => (&rest[..len], &rest[(MARK_CLASS.len_utf8() + len)..], true),
                    None => (rest, &rest[rest.len()..], false),
                };
                let id = text::validate_identifier(id)
                    .map_err(|error| StaticError::Id { error })?;
                (Some(id), rest, expect_class)
            } else if rest.starts_with(MARK_CLASS) {
                (None, &rest[MARK_CLASS.len_utf8() ..], true)
            } else {
                (None, rest, false)
            };
        let classes =
            if expect_class {
                for (index, class) in rest.split(MARK_CLASS).enumerate() {
                    text::validate_identifier(class)
                        .map_err(|error| StaticError::Class { index, error })?;
                }
                Some(rest)
            } else {
                None
            };
        match (tag, id, classes) {
            (None, None, None) => Err(StaticError::Unrestricted),
            (tag, id, classes) => Ok(Static { tag, id, classes }),
        }
    }
}

#[derive(Debug)]
pub enum Error {
    Tag {
        error: text::IdentifierError,
    },
    Id {
        error: text::IdentifierError,
    },
    Class {
        error: text::IdentifierError,
    },
    Classes {
        error: ClassesError,
    },
}

impl fmt::Display for Error {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::Id { .. } =>
                write!(fmt, "Invalid identifier for id selector"),
            Error::Tag { .. } =>
                write!(fmt, "Invalid identifier for tag name selector"),
            Error::Class { .. } =>
                write!(fmt, "Invalid identifier for class selector"),
            Error::Classes { .. } =>
                write!(fmt, "Invalid classes selector"),
        }
    }
}

impl error::Error for Error {

    fn description(&self) -> &str { "selector error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::Id { ref error }
            | Error::Tag { ref error }
            | Error::Class { ref error } => Some(error),
            Error::Classes { ref error } => Some(error),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn static_str() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a>",
            "<a><b></b></a>",
            |html| html.select("b", |html| html.remove_contents())
        );
        test_transform!(
            Default::default(),
            "<a><b id=\"x\">23</b><b id=\"y\">45</b></a>",
            "<a><b id=\"x\">23</b><b id=\"y\"></b></a>",
            |html| html.select("#y", |html| html.remove_contents())
        );
        test_transform!(
            Default::default(),
            "<a><b class=\"w x\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"w x\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".y", |html| html.remove_contents())
        );
        test_transform!(
            Default::default(),
            "<a><b class=\"x y\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"x y\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".y.z", |html| html.remove_contents())
        );
        test_transform!(
            Default::default(),
            "<a><b class=\"x y\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"x y\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".z.y", |html| html.remove_contents())
        );
        test_transform!(
            Default::default(),
            "<a>A</a><a id=\"x\">B</a><a class=\"y z\">C</a><a id=\"x\" class=\"y z\">D</a>",
            "<a>A</a><a id=\"x\">B</a><a class=\"y z\">C</a><a id=\"x\" class=\"y z\"></a>",
            |html| html.select("a#x.z.y", |html| html.remove_contents())
        );
    }

    #[test]
    fn classes() {
        test_transform!(
            Default::default(),
            "<a class=\"X\">23</a><b class=\"Y X\">42</b><a class=\"X Y\">99</a><a>33</a>",
            "<a class=\"X\">23</a><b class=\"Y X\"></b><a class=\"X Y\"></a><a>33</a>",
            |html| html
                .select(
                    super::Classes::from_str_iterator(["x", "Y"].iter().cloned()).unwrap(),
                    |html| html.remove_contents()
                )
        );
        test_stream_error!(
            Default::default(),
            "",
            ::event::StreamError::Selector {
                error: super::Error::Classes {
                    error: super::ClassesError::Identifier {
                        index: 2,
                        error: ::text::IdentifierError::Empty,
                    },
                },
            },
            |html| html.select(
                super::Classes::from_str_iterator(["x", "y", "", "z"].iter().cloned()),
                |html| html,
            )
        );
        let empty: &[&'static str] = &[];
        test_stream_error!(
            Default::default(),
            "",
            ::event::StreamError::Selector {
                error: super::Error::Classes {
                    error: super::ClassesError::Empty,
                },
            },
            |html| html.select(
                super::Classes::from_str_iterator(empty.iter().cloned()),
                |html| html,
            )
        );
    }

    #[test]
    fn class() {
        test_stream_error!(
            Default::default(),
            "",
            ::event::StreamError::Selector {
                error: super::Error::Class {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Class::from_str(""), |html| html)
        );
        test_transform!(
            Default::default(),
            "<a class=\"X\">23</a><b class=\"Y\">42</b><a class=\"X Y\">99</a><a>33</a>",
            "<a class=\"X\"></a><b class=\"Y\">42</b><a class=\"X Y\"></a><a>33</a>",
            |html| html
                .select(super::Class::from_str("x").unwrap(), |html| html
                    .remove_contents()
                )
        );
    }

    #[test]
    fn id() {
        test_stream_error!(
            Default::default(),
            "",
            ::event::StreamError::Selector {
                error: super::Error::Id {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Id::from_str(""), |html| html)
        );
        test_transform!(
            Default::default(),
            "<a id=\"X\">23</a><b id=\"Y\">42</b><a id=\"X\">99</a><a>33</a>",
            "<a id=\"X\"></a><b id=\"Y\">42</b><a id=\"X\"></a><a>33</a>",
            |html| html
                .select(super::Id::from_str("x").unwrap(), |html| html
                    .remove_contents()
                )
        );
    }

    #[test]
    fn tag() {
        test_stream_error!(
            Default::default(),
            "",
            ::event::StreamError::Selector {
                error: super::Error::Tag {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Tag::from_str(""), |html| html)
        );
        test_transform!(
            Default::default(),
            "<a>23</a><b>42</b><a>99</a>",
            "<a></a><b>42</b><a></a>",
            |html| html
                .select(super::Tag::from_str("a").unwrap(), |html| html
                    .remove_contents()
                )
        );
    }
}
