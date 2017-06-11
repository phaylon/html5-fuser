
use std::str;
use std::rc;

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

impl<'a> Selector for &'a Selector {
    
    fn matches(&self, tag: &text::Identifier, attributes: &event::Attributes) -> bool {
        (*self).matches(tag, attributes)
    }
}

pub trait IntoSelector {

    type Selector: Selector;

    fn into_selector(self) -> Self::Selector;
}

impl<T> IntoSelector for T where T: Selector {

    type Selector = Self;

    fn into_selector(self) -> Self { self }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Tag {
    identifier: text::Identifier,
}

impl str::FromStr for Tag {

    type Err = Error;

    fn from_str(value: &str) -> Result<Tag, Self::Err> {
        Ok(Tag {
            identifier: value.parse()?,
        })
    }
}

#[derive(Debug)]
pub enum Error {
    Identifier {
        error: text::IdentifierError,
    },
}

impl From<text::IdentifierError> for Error {

    fn from(error: text::IdentifierError) -> Error {
        Error::Identifier { error }
    }
}

impl Selector for Tag {

    fn matches(&self, tag: &text::Identifier, _attributes: &event::Attributes) -> bool {
        *tag == self.identifier
    }
}
