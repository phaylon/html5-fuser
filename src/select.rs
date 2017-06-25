
//! Stream selection functionality.
//!
//! This functionality is used when substreams are selected, via `Api::select` and similar.
//!
//! These methods usually take anything implementing `IntoSelector`. That allows supplying
//! pre-built and verified selectors, selector construction results and static strings. For
//! results and static selectors any selector errors will be propagated through the stream,
//! halting the transform.

use std::str;
use std::rc;
use std::fmt;
use std::error;

use text;
use event;

/// Trait implemented by all selectors.
pub trait Selector {

    /// Determines if the given event matches the selector.
    fn matches(&self, event: &event::Event) -> bool;
}

impl<T> Selector for rc::Rc<T> where T: Selector {
    
    fn matches(&self, event: &event::Event) -> bool {
        (**self).matches(event)
    }
}

impl<'a, T> Selector for &'a T where T: Selector {
    
    fn matches(&self, event: &event::Event) -> bool {
        (*self).matches(event)
    }
}

/// Conversion trait for selectors.
///
/// This trait is used by `Api::select` and similar methods to construct selectors from
/// provided values.
pub trait IntoSelector {

    /// The kind of error that is returned when a conversion fails.
    type Error: Into<event::StreamError>;

    /// The kind of selector that is returned on success.
    type Selector: Selector;

    /// Convert the value into a selector.
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

/// Id construction error.
#[derive(Debug)]
pub enum IdError {
    /// The ID identifier was invalid.
    Identifier {
        /// Reason for the identifier invalidity.
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

/// Select all elements with a specific ID.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::select::{ Id };
///
/// // For the Id construction.
/// use std::str::{ FromStr };
///
/// let template = Template::from_str(r#"
///     <a id="home-link">Home</a>
///     <a id="contact-link">Contact</a>
/// "#, ParseOptions::default())?;
///
/// let home_id = Id::from_str("home-link")?;
///
/// let output = format!("{}", template.transform(|html| html
///     // Using a preconstructed id selector.
///     .select(home_id, |html| html
///         .set_attribute_with_value("href", "index.html")
///     )
///     // Using a id selector construction result.
///     // A selector error like an invalid identifier
///     // will emit a stream error.
///     .select(Id::from_str("contact-link"), |html| html
///         .set_attribute_with_value("href", "contact.html")
///     )
/// )?);
///
/// assert!(output.contains(r#"id="home-link" href="index.html""#));
/// assert!(output.contains(r#"id="contact-link" href="contact.html""#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
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

    fn matches(&self, event: &event::Event) -> bool {
        if let Some(attributes) = event.attributes() {
            attributes.has_id(&self.identifier)
        } else {
            false
        }
    }
}

/// Tag construction error.
#[derive(Debug)]
pub enum TagError {
    /// The tag selector had an invalid identifier.
    Identifier {
        /// Reason for the identifier invalidity.
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

/// Select all elements with a specific tag name.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::select::{ Tag };
///
/// // For the Tag construction.
/// use std::str::{ FromStr };
///
/// let template = Template::from_str(r#"
///     <html>
///         <head>
///             <title></title>
///         </head>
///         <body/>
///     </html>
/// "#, ParseOptions::default())?;
///
/// let title_tag = Tag::from_str("title")?;
///
/// let output = format!("{}", template.transform(|html| html
///     // Using a preconstructed tag name selector.
///     .select(title_tag, |html| html
///         .replace_contents("New Title")
///     )
///     // Using a tag name selector construction result.
///     // A selector error like an invalid identifier
///     // will emit a stream error.
///     .select(Tag::from_str("body"), |html| html
///         .replace_contents("Content")
///     )
/// )?);
///
/// assert!(output.contains(r#"<title>New Title</title>"#));
/// assert!(output.contains(r#"<body>Content</body>"#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
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

    fn matches(&self, event: &event::Event) -> bool {
        if let Some(tag) = event.element_tag_start() {
            *tag == self.identifier
        } else {
            false
        }
    }
}

/// Class construction error.
#[derive(Debug)]
pub enum ClassError {
    /// The class identifier was invalid.
    Identifier {
        /// Reason for identifier invalidity.
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

/// Select all elements with a specific class.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::select::{ Class };
///
/// // For the Class construction.
/// use std::str::{ FromStr };
///
/// let template = Template::from_str(r#"
///     <a class="home-link">Home</a>
///     <a class="contact-link">Contact</a>
/// "#, ParseOptions::default())?;
///
/// let home_class = Class::from_str("home-link")?;
///
/// let output = format!("{}", template.transform(|html| html
///     // Using a preconstructed class selector.
///     .select(home_class, |html| html
///         .set_attribute_with_value("href", "index.html")
///     )
///     // Using a class selector construction result.
///     // A selector error like an invalid identifier
///     // will emit a stream error.
///     .select(Class::from_str("contact-link"), |html| html
///         .set_attribute_with_value("href", "contact.html")
///     )
/// )?);
///
/// assert!(output.contains(r#"class="home-link" href="index.html""#));
/// assert!(output.contains(r#"class="contact-link" href="contact.html""#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
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

    fn matches(&self, event: &event::Event) -> bool {
        if let Some(attributes) = event.attributes() {
            attributes.has_class(&self.identifier)
        } else {
            false
        }
    }
}

/// Classes construction error.
#[derive(Debug)]
pub enum ClassesError {
    /// The class list was empty, making the selector unrestricted.
    Empty,
    /// One of the class names was not a valid identifier.
    Identifier {
        /// Index of the invalid identifier.
        index: usize,
        /// Reason for the identifier invalidity.
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

/// Select all elements having all the specified classes.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::select::{ Classes };
///
/// // For the Classes construction.
/// use std::str::{ FromStr };
///
/// let template = Template::from_str(r#"
///     <a class="home navlink">Home</a>
///     <a class="contact navlink">Contact</a>
/// "#, ParseOptions::default())?;
///
/// let home_classes = Classes::from_str_iterator(
///     "home navlink".split_whitespace(),
/// )?;
///
/// let output = format!("{}", template.transform(|html| html
///     // Using a preconstructed classes selector.
///     .select(home_classes, |html| html
///         .set_attribute_with_value("href", "index.html")
///     )
///     // Using a classes selector construction result.
///     // A selector error like an invalid identifier
///     // will emit a stream error.
///     .select(
///         Classes::from_str_iterator(
///             "navlink contact".split_whitespace()
///         ),
///         |html| html.set_attribute_with_value("href", "contact.html"),
///     )
/// )?);
///
/// assert!(
///     output
///     .contains(r#"class="home navlink" href="index.html""#)
/// );
/// assert!(
///     output
///     .contains(r#"class="contact navlink" href="contact.html""#)
/// );
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
#[derive(Debug, Clone)]
pub struct Classes {
    identifiers: Vec<text::Identifier>,
}

impl Classes {

    /// Construct an instance from an iterator of `&str`s.
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

    fn matches(&self, event: &event::Event) -> bool {
        if let Some(attributes) = event.attributes() {
            for identifier in &self.identifiers {
                if !attributes.has_class(identifier) {
                    return false;
                }
            }
            true
        } else {
            false
        }
    }
}

/// Static selector errors.
#[derive(Debug)]
pub enum StaticError {
    /// The selector was unrestricted.
    Unrestricted,
    /// The tag name selector had an invalid identifier.
    Tag {
        /// Reason for the identifier invalidity.
        error: text::IdentifierError,
    },
    /// The id selector had an invalid identifier.
    Id {
        /// Reason for the identifier invalidity.
        error: text::IdentifierError,
    },
    /// A class selector had an invalid identifier.
    Class {
        /// Index of the invalid class.
        index: usize,
        /// Reason for the identifier invalidity.
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

/// A selector based on a `&'static str` specification.
///
/// This is a convenience selector that allows specifying the tag name, ID and classes
/// for the selector in a single `&'static str`. The value has the following parts, in
/// order:
///
/// * An optional bare identifier for the tag name. For example: `body`.
/// * An optional `#` prefixed identifier for the ID. For example: `#page-title`.
/// * Zero or more `.` prefixed identifiers for required classes. For example: `.navlink`.
///
/// Empty and thus unrestricted selectors are invalid. At least one of the above parts has
/// to be included.
///
/// The parts are put right after each other. So `a#home.link.active` would match any `a`
/// element with a `home` ID and both `link` and `active` classes.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
///
/// let template = Template::from_str(r#"
///     <html>
///         <head><title>Title</title></head>
///         <body>
///             <div id="message">
///                 Hello
///                 <span class="current-user">Current User</span>!
///             </div>
///         </body>
///     </html>
/// "#, ParseOptions::default())?;
///
/// let output = format!("{}", template.transform(|html| html
///     .select("title", |html| html.replace_contents("Welcome Page"))
///     .select("#message", |html| html
///         .subselect(".current-user", |html| html
///             .replace_contents("Foo")
///         )
///     )
/// )?);
///
/// assert!(output.contains(r#"<title>Welcome Page</title>"#));
/// assert!(output.contains(r#"<span class="current-user">Foo</span>"#));
///
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
#[derive(Debug, Clone)]
pub struct Static {
    tag: Option<&'static str>,
    id: Option<&'static str>,
    classes: Option<&'static str>,
}

impl Selector for Static {

    fn matches(&self, event: &event::Event) -> bool {
        let (tag, attributes) = match (event.element_tag_start(), event.attributes()) {
            (Some(tag), Some(attributes)) => (tag, attributes),
            _ => return false,
        };
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

/// Error used in event stream emission.
///
/// Static selector errors are emitted via `StaticError` instead.
#[derive(Debug)]
pub enum Error {
    /// A tag name selector had an invalid identifier.
    Tag {
        /// Reason for the identifier invalidity.
        error: text::IdentifierError,
    },
    /// A ID selector had an invalid identifier.
    Id {
        /// Reason for the identifier invalidity.
        error: text::IdentifierError,
    },
    /// A class selector had an invalid identifier.
    Class {
        /// Reason for the identifier invalidity.
        error: text::IdentifierError,
    },
    /// A multiple classes selector was invalid.
    Classes {
        /// Reason for the selector invalidity.
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

    const EMPTY_STR_LIST: &[&str] = &[];

    test_group!(static_str:
        "tag" => transform_test!(
            "<a><b>23</b></a>",
            "<a><b></b></a>",
            |html| html.select("b", |html| html.remove_contents()),
        ),
        "tag identifier error" => transform_error_test!(
            "<a/>",
            ::event::StreamError::StaticSelector {
                error: super::StaticError::Tag {
                    error: ::text::IdentifierError::Forbidden { forbidden: '/' },
                },
            },
            |html| html.select("/", |html| html),
        ),
        "id" => transform_test!(
            "<a><b id=\"x\">23</b><b id=\"y\">45</b></a>",
            "<a><b id=\"x\">23</b><b id=\"y\"></b></a>",
            |html| html.select("#y", |html| html.remove_contents()),
        ),
        "id identifier error" => transform_error_test!(
            "<a/>",
            ::event::StreamError::StaticSelector {
                error: super::StaticError::Id {
                    error: ::text::IdentifierError::Forbidden { forbidden: '/' },
                },
            },
            |html| html.select("#/", |html| html),
        ),
        "class" => transform_test!(
            "<a><b class=\"w x\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"w x\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".y", |html| html.remove_contents()),
        ),
        "class identifier error" => transform_error_test!(
            "<a/>",
            ::event::StreamError::StaticSelector {
                error: super::StaticError::Class {
                    index: 2,
                    error: ::text::IdentifierError::Forbidden { forbidden: '/' },
                },
            },
            |html| html.select(".x.y./", |html| html),
        ),
        "multiple classes" => transform_test!(
            "<a><b class=\"x y\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"x y\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".y.z", |html| html.remove_contents()),
        ),
        "multiple classes reverse" => transform_test!(
            "<a><b class=\"x y\">23</b><b class=\"y z\">45</b></a>",
            "<a><b class=\"x y\">23</b><b class=\"y z\"></b></a>",
            |html| html.select(".z.y", |html| html.remove_contents()),
        ),
        "tag, id, classes" => transform_test!(
            "<a>A</a><a id=\"x\">B</a><a class=\"y z\">C</a><a id=\"x\" class=\"y z\">D</a>",
            "<a>A</a><a id=\"x\">B</a><a class=\"y z\">C</a><a id=\"x\" class=\"y z\"></a>",
            |html| html.select("a#x.z.y", |html| html.remove_contents()),
        ),
        "unrestricted error" => transform_error_test!(
            "<a/>",
            ::event::StreamError::StaticSelector {
                error: super::StaticError::Unrestricted,
            },
            |html| html.select("", |html| html),
        ),
    );

    test_group!(classes:
        "from_str_iterator" => transform_test!(
            "<a class=\"X\">23</a><b class=\"Y X\">42</b><a class=\"X Y\">99</a><a>33</a>",
            "<a class=\"X\">23</a><b class=\"Y X\"></b><a class=\"X Y\"></a><a>33</a>",
            |html| html.select(
                super::Classes::from_str_iterator(["x", "Y"].iter().cloned()).unwrap(),
                |html| html.remove_contents()
            ),
        ),
        "from_str_iterator identifier error" => transform_error_test!(
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
            ),
        ),
        "from_str_iterator empty list" => transform_error_test!(
            "",
            ::event::StreamError::Selector {
                error: super::Error::Classes {
                    error: super::ClassesError::Empty,
                },
            },
            |html| html.select(
                super::Classes::from_str_iterator(EMPTY_STR_LIST.iter().cloned()),
                |html| html,
            )
        ),
    );

    test_group!(class:
        "from_str" => transform_test!(
            "<a class=\"X\">23</a><b class=\"Y\">42</b><a class=\"X Y\">99</a><a>33</a>",
            "<a class=\"X\"></a><b class=\"Y\">42</b><a class=\"X Y\"></a><a>33</a>",
            |html| html.select(super::Class::from_str("x").unwrap(), |html| html
                .remove_contents()
            ),
        ),
        "from_str identifier error" => transform_error_test!(
            "",
            ::event::StreamError::Selector {
                error: super::Error::Class {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Class::from_str(""), |html| html),
        ),
    );

    test_group!(id:
        "from_str" => transform_test!(
            "<a id=\"X\">23</a><b id=\"Y\">42</b><a id=\"X\">99</a><a>33</a>",
            "<a id=\"X\"></a><b id=\"Y\">42</b><a id=\"X\"></a><a>33</a>",
            |html| html.select(super::Id::from_str("x").unwrap(), |html| html
                .remove_contents()
            ),
        ),
        "from_str identifier error" => transform_error_test!(
            "",
            ::event::StreamError::Selector {
                error: super::Error::Id {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Id::from_str(""), |html| html),
        ),
    );

    test_group!(tag:
        "from_str" => transform_test!(
            "<a>23</a><b>42</b><a>99</a>",
            "<a></a><b>42</b><a></a>",
            |html| html.select(super::Tag::from_str("a").unwrap(), |html| html
                .remove_contents()
            ),
        ),
        "from_str identifier error" => transform_error_test!(
            "",
            ::event::StreamError::Selector {
                error: super::Error::Tag {
                    error: ::text::IdentifierError::Empty,
                },
            },
            |html| html.select(super::Tag::from_str(""), |html| html)
        ),
    );
}
