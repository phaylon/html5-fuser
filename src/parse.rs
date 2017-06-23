
//! HTML5 parsing functionality.
//!
//! Note that this parser only understands HTML5 as far as deemed necessary, since this library
//! is primarily intended for templating. For example, identifiers that are accepted are limited
//! to ASCII excluding some specific characters.

use std::fmt;
use std::error;

use input;
use location;
use event;
use text;

/// Parsing options.
///
/// # Examples
///
/// ```
/// # use html5_fuser::parse::{ Options };
/// // via Default::default()
/// let options_default = Options::default();
///
/// let options_default_adjusted = Options::default()
///     .max_stack_depth(128)
///     .max_input_len(32_768);
///
/// let options_direct = Options {
///     max_stack_depth: Some(128),
///     max_input_len: Some(32_768),
/// };
/// ```
#[derive(Debug, Clone, Copy)]
pub struct Options {
    /// A limit for the depth of the HTML5 tree.
    pub max_stack_depth: Option<usize>,
    /// A limit for the length of the input data.
    pub max_input_len: Option<usize>,
}

impl Options {

    /// Set a maximum tree stack depth.
    pub fn max_stack_depth(mut self, max_depth: usize) -> Self {
        self.max_stack_depth = Some(max_depth);
        self
    }

    /// Set the tree stack depth to unlimited.
    pub fn no_max_stack_depth(mut self) -> Self {
        self.max_stack_depth = None;
        self
    }

    /// Set a maximum input length.
    pub fn max_input_len(mut self, max_len: usize) -> Self {
        self.max_input_len = Some(max_len);
        self
    }

    /// Set the input length to unlimited.
    pub fn no_max_input_len(mut self) -> Self {
        self.max_input_len = None;
        self
    }
}

impl Default for Options {

    fn default() -> Options {
        Options {
            max_stack_depth: None,
            max_input_len: None,
        }
    }
}

/// Parsing error.
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
    /// A doctype was parsed that is not supported by this parser.
    ///
    /// Note: Only `html` is supported.
    UnsupportedDoctype {
        /// Location of the doctype.
        location: location::Location,
        /// The invalid doctype value.
        doctype: String,
    },
    /// A doctype directive was started but never finished.
    UnclosedDoctype {
        /// Location where the doctype started.
        location: location::Location,
    },
    /// A comment was started but never finished.
    UnclosedComment {
        /// Location where the comment started.
        location: location::Location,
    },
    /// An opening tag was started but never finished.
    ExpectedOpeningTagClose {
        /// Name of the unfinished tag.
        tag: String,
        /// Location where the tag was started.
        location: location::Location,
    },
    /// A closing tag was started but never finished.
    ExpectedClosingTagClose {
        /// Name of the unfinished tag.
        tag: String,
        /// Location where the tag was started.
        location: location::Location,
    },
    /// An identifier at the beginning of a tag was expected.
    ExpectedOpeningTagIdentifier {
        /// Location where the identifier could not be found.
        location: location::Location,
    },
    /// An identifier at the beginning of a tag was invalid.
    InvalidOpeningTagIdentifier {
        /// Location of the invalid identifier.
        location: location::Location,
        /// Reason the identifier was invalid.
        identifier_error: text::IdentifierError,
    },
    /// An identifier at the beginning of a closing tag was expected.
    ExpectedClosingTagIdentifier {
        /// The location where the identifier could not be found
        location: location::Location,
    },
    /// An identifier at the beginning of a closing tag was invalid.
    InvalidClosingTagIdentifier {
        /// Location of the invalid identifier.
        location: location::Location,
        /// Reason the identifier was invalid.
        identifier_error: text::IdentifierError,
    },
    /// An attribute name identifier was invalid.
    InvalidAttributeIdentifier {
        /// Location of the invalid identifier.
        location: location::Location,
        /// Reason the identifier was invalid.
        identifier_error: text::IdentifierError,
    },
    /// An attribute value was not enclosed in quotes.
    InvalidAttributeValue {
        /// Location of the invalid attribute value.
        location: location::Location,
        /// Name of the tag with the invalid attribute value.
        tag: String,
        /// Name of the attribute with the invalid value.
        attribute_name: String,
    },
    /// An attribute value was started but not finished.
    UnclosedAttributeValue {
        /// Location of the unclosed attribute value.
        location: location::Location,
        /// Name of the tag with the unclosed attribute value.
        tag: String,
        /// Name of the attribute with the unclosed value.
        attribute_name: String,
    },
    /// A part of the input could not be parsed at all.
    UnexpectedInput {
        /// Location of the unparsable input.
        location: location::Location,
    },
    /// The end of the input was reached but some elements were not closed.
    MissingClosingTags {
        /// The innermost missing closing tag identifier and its location.
        inner: (String, location::Location),
        /// The rest of the missing closing tag identifiers and their location.
        others: Vec<(String, location::Location)>,
    },
    /// The stack limit set in the parsing options was exceeded.
    StackLimitExceeded {
        /// Tag that exceeded the limit.
        tag: String,
        /// Location where the limit was exceeded.
        location: location::Location,
        /// The configured limit.
        limit: usize,
    },
    /// A lone closing tag was found at the top level.
    UnexpectedClosingTag {
        /// Name of the tag.
        tag: String,
        /// Location where the closing tag occured.
        location: location::Location,
    },
    /// A closing tag was found, but a different element is currently open.
    MismatchedClosingTag {
        /// Name of the opening tag.
        opening_tag: String,
        /// Location where the opening tag occured.
        opening_location: location::Location,
        /// Name of the closing tag.
        closing_tag: String,
        /// Location where the mismatched closing tag was encountered.
        closing_location: location::Location,
    },
}

impl error::Error for Error {

    fn description(&self) -> &str { "Parse error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::InvalidOpeningTagIdentifier { ref identifier_error, .. }
            | Error::InvalidClosingTagIdentifier { ref identifier_error, .. } 
            | Error::InvalidAttributeIdentifier { ref identifier_error, .. } =>
                Some(identifier_error),
            _ => None,
        }
    }
}

impl fmt::Display for Error {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Error::UnsupportedDoctype { ref doctype, location } =>
                write!(fmt, "Unsupported doctype '{}' at {}", doctype, location),
            Error::UnclosedDoctype { location } =>
                write!(fmt, "Unclosed doctype directive at {}", location),
            Error::UnclosedComment { location } =>
                write!(fmt, "Unclosed comment directive at {}", location),
            Error::ExpectedOpeningTagClose { ref tag, location } =>
                write!(fmt, "Unfinished opening tag '{}' at {}", tag, location),
            Error::ExpectedClosingTagClose { ref tag, location } =>
                write!(fmt, "Unfinished closing tag '{}' at {}", tag, location),
            Error::ExpectedOpeningTagIdentifier { location } =>
                write!(fmt, "Expected an identifier for opening tag at {}", location),
            Error::ExpectedClosingTagIdentifier { location } =>
                write!(fmt, "Expected an identifier for closing tag at {}", location),
            Error::InvalidOpeningTagIdentifier { location, .. } =>
                write!(fmt, "Invalid opening tag identifier at {}", location),
            Error::InvalidClosingTagIdentifier { location, .. } =>
                write!(fmt, "Invalid opening tag identifier at {}", location),
            Error::InvalidAttributeIdentifier { location, .. } =>
                write!(fmt, "Invalid attribute identifier at {}", location),
            Error::InvalidAttributeValue { ref tag, ref attribute_name, location } =>
                write!(fmt, "Invalid attribute value for '{}.{}' at {}",
                    tag,
                    attribute_name,
                    location,
                ),
            Error::UnclosedAttributeValue { ref tag, ref attribute_name, location } =>
                write!(fmt, "Unclosed attribute value for '{}.{}' at {}",
                    tag,
                    attribute_name,
                    location,
                ),
            Error::UnexpectedInput { location } =>
                write!(fmt, "Unexpected input at {}", location),
            Error::MissingClosingTags { inner: (ref tag, location), ref others } =>
                {
                    write!(fmt, "Unclosed '{}' tag starting at {}", tag, location)?;
                    if !others.is_empty() {
                        write!(fmt, "(also unclosed are: ")?;
                        for (index, &(ref tag, _)) in others.iter().enumerate() {
                            if index != 0 {
                                write!(fmt, ",")?;
                            }
                            write!(fmt, "'{}'", tag)?;
                        }
                        write!(fmt, ")")?;
                    }
                    Ok(())
                },
            Error::StackLimitExceeded { location, ref tag, limit } =>
                write!(fmt, "Stack limit ({}) exceeded by tag '{}' at {}",
                    limit,
                    tag,
                    location,
                ),
            Error::UnexpectedClosingTag { location, ref tag } =>
                write!(fmt, "Unexpected closing tag '{}' at {}", tag, location),
            Error::MismatchedClosingTag {
                ref opening_tag,
                ref closing_tag,
                opening_location,
                closing_location,
            } =>
                write!(fmt, "Closing tag '{}' at {} does not match opening tag '{}' at {}",
                    opening_tag,
                    opening_location,
                    closing_tag,
                    closing_location,
                ),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Iter<'i> {
    rest: input::Input<'i>,
    mode: Mode,
    stack: Vec<location::Located<text::Identifier>>,
    options: Options,
}

impl<'i> Iterator for Iter<'i> {

    type Item = Result<event::Event, Error>;
    
    fn next(&mut self) -> Option<Self::Item> {

        if self.rest.is_empty() {
            if let Some(entry) = self.stack.pop() {
                return Some(Err(Error::MissingClosingTags {
                    inner: (entry.value.to_string(), entry.location),
                    others: self.stack
                        .iter()
                        .map(|entry| (entry.value.to_string(), entry.location))
                        .collect(),
                }));
            }
            return None;
        }

        let Parsed { event, next_mode: new_mode, rest: new_rest } =
            match parse(self.mode, self.rest) {
                Ok(value) => value,
                Err(error) => {
                    self.finalize();
                    return Some(Err(error));
                },
            };
        let location::Located { value: event::Event(event), location } = event;

        if let event::EventKind::OpeningTag { ref tag, .. } = event {
            if let Some(limit) = self.options.max_stack_depth {
                if limit == self.stack.len() {
                    self.finalize();
                    return Some(Err(Error::StackLimitExceeded {
                        location, 
                        tag: tag.to_string(),
                        limit,
                    }));
                }
            }
            self.stack.push(location.wrap(tag.clone()));
        }

        if let event::EventKind::ClosingTag { ref tag, .. } = event {
            match self.stack.pop() {
                None => {
                    self.finalize();
                    return Some(Err(Error::UnexpectedClosingTag {
                        location,
                        tag: tag.to_string(),
                    }));
                },
                Some(entry) =>
                    if *tag != entry.value {
                        self.finalize();
                        return Some(Err(Error::MismatchedClosingTag {
                            opening_location: entry.location,
                            opening_tag: entry.value.to_string(),
                            closing_location: location,
                            closing_tag: tag.to_string(),
                        }));
                    },
            }
        }

        self.rest = new_rest;
        self.mode = new_mode;

        Some(Ok(event::Event(event)))
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum Mode {
    Data,
    RawData(&'static str),
}

struct Parsed<'i> {
    event: location::Located<event::Event>,
    next_mode: Mode,
    rest: input::Input<'i>,
}

type ParseResult<'i> = Result<Parsed<'i>, Error>;

impl<'i> Iter<'i> {

    pub fn new(input: &'i str, options: Options) -> Iter<'i> {
        Iter {
            rest: input::Input::new(input),
            mode: Mode::Data,
            stack: Vec::new(),
            options,
        }
    }

    fn finalize(&mut self) {
        self.rest = self.rest.end();
    }
}

fn try_parse_tag(input: input::Input) -> Option<ParseResult> {
    try_parse_tag_close(input).or_else(|| try_parse_tag_open(input))
}

fn parse_data_mode(input: input::Input) -> ParseResult {
    try_parse_comment(input)
        .or_else(|| try_parse_doctype(input))
        .or_else(|| try_parse_tag(input))
        .or_else(|| try_parse_data(input))
        .unwrap_or_else(|| Err(Error::UnexpectedInput { location: input.location() }))
}

fn parse(mode: Mode, input: input::Input) -> ParseResult {
    match mode {
        Mode::Data => parse_data_mode(input),
        Mode::RawData(tag) => parse_raw_data(input, tag),
    }
}

fn parse_raw_data<'i>(input: input::Input<'i>, searched: &str) -> ParseResult<'i> {

    let (content, rest) =
        input.take_until_end_char('<', |rest| {
            try_parse_tag_close(rest)
                .and_then(Result::ok)
                .map(|parsed| parsed.event.value.is_closing_tag_for_str(searched))
                .unwrap_or(false)
        })
        .unwrap_or_else(|| input.take_all());

    Ok(Parsed {
        event: input.location().wrap(event::raw_data(text::EncodedText::from_raw(content))),
        next_mode: Mode::Data,
        rest,
    })
}

fn try_parse_data(input: input::Input) -> Option<ParseResult> {

    let (data, rest) = input
        .take_until_char('<')
        .unwrap_or_else(|| input.take_all());

    if data.is_empty() {
        return None;
    }
    
    Some(Ok(Parsed {
        event: input.location().wrap(event::data(text::Data::from_raw(data))),
        next_mode: Mode::Data,
        rest,
    }))
}

fn try_parse_comment(input: input::Input) -> Option<ParseResult> {

    let rest = match input.take_str("<!--") {
        Some(rest) => rest,
        None => return None,
    };

    let (content, rest) = match rest.take_until_str("-->") {
        Some((content, rest)) => (content, rest.take_str("-->").expect("comment end")),
        None => return Some(Err(Error::UnclosedComment {
            location: input.location(),
        })),
    };

    Some(Ok(Parsed {
        event: input.location().wrap(event::comment(text::EncodedText::from_raw(content))),
        next_mode: Mode::Data,
        rest,
    }))
}

fn try_parse_doctype(input: input::Input) -> Option<ParseResult> {

    let rest = match input.take_str("<!") {
        Some(rest) => rest.skip_whitespace(),
        None => return None,
    };
    let rest = match rest.try_take_exact_identifier("doctype") {
        Some((_, rest)) => rest.skip_whitespace(),
        None => return None,
    };
    let (content, rest) = match rest.take_until_char('>') {
        Some((content, rest)) => (content.trim(), rest.take_char('>').expect("close type")),
        None => return Some(Err(Error::UnclosedDoctype {
            location: input.location(),
        })),
    };

    if !text::identifier_eq(content, "html") {
        return Some(Err(Error::UnsupportedDoctype {
            location: input.location(), 
            doctype: content.into(),
        }));
    }

    Some(Ok(Parsed {
        event: input.location().wrap(event::doctype(text::EncodedText::from_raw(content))),
        next_mode: Mode::Data,
        rest,
    }))
}

fn try_parse_tag_close(input: input::Input) -> Option<ParseResult> {

    let rest = match input.take_char('<') {
        Some(rest) => rest.skip_whitespace(),
        None => return None,
    };
    let rest = match rest.take_char('/') {
        Some(rest) => rest.skip_whitespace(),
        None => return None,
    };
    let (identifier, rest) = match rest.take_identifier() {
        Ok(Some((identifier, rest))) => (identifier, rest.skip_whitespace()),
        Ok(None) => return Some(Err(Error::ExpectedClosingTagIdentifier {
            location: rest.location(),
        })),
        Err(error) => return Some(Err(Error::InvalidClosingTagIdentifier {
            identifier_error: error,
            location: rest.location(),
        })),
    };
    let rest = match rest.take_char('>') {
        Some(rest) => rest,
        None => return Some(Err(Error::ExpectedClosingTagClose {
            tag: (*identifier).into(),
            location: rest.location(),
        })),
    };

    Some(Ok(Parsed {
        event: input.location().wrap(event::close(identifier)),
        next_mode: Mode::Data,
        rest,
    }))
}

struct AttributeData<'i> {
    attribute: event::Attribute,
    rest: input::Input<'i>,
}

fn try_parse_attribute<'i>(tag: &str, input: input::Input<'i>)
-> Result<Option<AttributeData<'i>>, Error> {

    let (name, rest) = match input.take_identifier() {
        Err(error) => return Err(Error::InvalidAttributeIdentifier {
            identifier_error: error,
            location: input.location(),
        }),
        Ok(None) => return Ok(None),
        Ok(Some((identifier, rest))) => (identifier, rest.skip_whitespace()),
    };

    let rest = match rest.take_char('=') {
        Some(rest) => rest.skip_whitespace(),
        None => return Ok(Some(AttributeData {
            attribute: event::Attribute::new(name, None),
            rest,
        })),
    };
    let value_start = rest.location();
    let rest = match rest.take_char('"') {
        Some(rest) => rest.skip_whitespace(),
        None => return Err(Error::InvalidAttributeValue { 
            location: value_start,
            tag: tag.into(),
            attribute_name: (*name).into(),
        }),
    };

    let (value, rest) = match rest.take_until_char('"') {
        Some((value, rest)) => (value, rest.take_char('"').expect("end marker")),
        None => return Err(Error::UnclosedAttributeValue { 
            location: value_start,
            tag: tag.into(),
            attribute_name: (*name).into(),
        }),
    };

    Ok(Some(AttributeData {
        attribute: event::Attribute::new(name, Some(text::Value::from_raw(value))),
        rest,
    }))
}

fn parse_attributes<'i>(tag: &str, input: input::Input<'i>)
-> Result<(event::Attributes, input::Input<'i>), Error> {

    let mut attributes = Vec::new();
    let mut rest = input.skip_whitespace();

    'search: while !rest.is_empty() {
        match try_parse_attribute(tag, rest)? {
            Some(AttributeData { attribute, rest: new_rest }) => {
                attributes.push(attribute);
                rest = new_rest.skip_whitespace();
            },
            None => break 'search,
        }
    }

    Ok((event::Attributes::new(attributes), rest))
}

fn try_parse_tag_open(input: input::Input) -> Option<ParseResult> {

    let rest = match input.take_char('<') {
        Some(rest) => rest.skip_whitespace(),
        None => return None,
    };
    let (tag, rest) = match rest.take_identifier() {
        Ok(Some((identifier, rest))) => (identifier, rest.skip_whitespace()),
        Ok(None) => return Some(Err(Error::ExpectedOpeningTagIdentifier {
            location: rest.location(),
        })),
        Err(error) => return Some(Err(Error::InvalidOpeningTagIdentifier {
            identifier_error: error,
            location: rest.location(),
        })),
    };

    let (attributes, rest) = match parse_attributes(&tag, rest) {
        Ok((attributes, rest)) => (attributes, rest.skip_whitespace()),
        Err(error) => return Some(Err(error)),
    };

    let (self_close, rest) = rest.take_char('/')
        .map(|rest| (true, rest.skip_whitespace()))
        .unwrap_or_else(|| (false, rest));

    let rest = match rest.take_char('>') {
        Some(rest) => rest,
        None => return Some(Err(Error::ExpectedOpeningTagClose {
            tag: (*tag).into(),
            location: rest.location(),
        })),
    };

    Some(Ok(Parsed {
        next_mode: mode_for_tag(&tag, self_close),
        rest,
        event: input.location().wrap(
            if is_void_tag(&tag) {
                event::void(tag, attributes)
            } else if self_close {
                event::self_closed(tag, attributes)
            } else {
                event::open(tag, attributes)
            },
        ),
    }))
}

fn mode_for_tag(tag: &str, self_close: bool) -> Mode {

    const RAW_TAGS: &'static [&'static str] =
        &["script", "style", "title", "textarea"];

    if !self_close {
        for &raw_tag in RAW_TAGS.iter() {
            if text::identifier_eq(tag, raw_tag) {
                return Mode::RawData(raw_tag);
            }
        }
    }

    Mode::Data
}

fn is_void_tag(tag: &str) -> bool {

    const VOID_TAGS: &'static [&'static str] = &[
        "area", "base", "br", "col", "embed", "hr", "img", "input", "keygen", "link",
        "menuitem", "meta", "param", "source", "track", "wbr",
    ];

    for &void_tag in VOID_TAGS.iter() {
        if text::identifier_eq(tag, void_tag) {
            return true;
        }
    }

    false
}

