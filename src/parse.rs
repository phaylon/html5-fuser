
use std::fmt;
use std::error;

use input;
use location;
use event;
use text;

#[derive(Debug, Clone, Copy)]
pub struct Options {
    pub max_stack_depth: Option<usize>,
    pub max_input_len: Option<usize>,
}

impl Options {

    pub fn max_stack_depth(mut self, max_depth: usize) -> Self {
        self.max_stack_depth = Some(max_depth);
        self
    }

    pub fn no_max_stack_depth(mut self) -> Self {
        self.max_stack_depth = None;
        self
    }

    pub fn max_input_len(mut self, max_len: usize) -> Self {
        self.max_input_len = Some(max_len);
        self
    }

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Error {
    UnsupportedDoctype {
        location: location::Location,
        doctype: String,
    },
    UnclosedDoctype {
        location: location::Location,
    },
    UnclosedComment {
        location: location::Location,
    },
    ExpectedOpeningTagClose {
        tag_name: String,
        location: location::Location,
    },
    ExpectedClosingTagClose {
        tag_name: String,
        location: location::Location,
    },
    ExpectedOpeningTagIdentifier {
        location: location::Location,
    },
    InvalidOpeningTagIdentifier {
        location: location::Location,
        identifier_error: text::IdentifierError,
    },
    ExpectedClosingTagIdentifier {
        location: location::Location,
    },
    InvalidClosingTagIdentifier {
        location: location::Location,
        identifier_error: text::IdentifierError,
    },
    InvalidAttributeIdentifier {
        location: location::Location,
        identifier_error: text::IdentifierError,
    },
    InvalidAttributeValue {
        location: location::Location,
        tag_name: String,
        attribute_name: String,
    },
    UnclosedAttributeValue {
        location: location::Location,
        tag_name: String,
        attribute_name: String,
    },
    UnexpectedInput {
        location: location::Location,
    },
    MissingClosingTags {
        inner: (String, location::Location),
        others: Vec<(String, location::Location)>,
    },
    StackLimitExceeded {
        tag_name: String,
        location: location::Location,
        limit: usize,
    },
    UnexpectedClosingTag {
        tag_name: String,
        location: location::Location,
    },
    MismatchedClosingTag {
        opening_tag_name: String,
        opening_location: location::Location,
        closing_tag_name: String,
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
            Error::ExpectedOpeningTagClose { ref tag_name, location } =>
                write!(fmt, "Unfinished opening tag '{}' at {}", tag_name, location),
            Error::ExpectedClosingTagClose { ref tag_name, location } =>
                write!(fmt, "Unfinished closing tag '{}' at {}", tag_name, location),
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
            Error::InvalidAttributeValue { ref tag_name, ref attribute_name, location } =>
                write!(fmt, "Invalid attribute value for '{}.{}' at {}",
                    tag_name,
                    attribute_name,
                    location,
                ),
            Error::UnclosedAttributeValue { ref tag_name, ref attribute_name, location } =>
                write!(fmt, "Unclosed attribute value for '{}.{}' at {}",
                    tag_name,
                    attribute_name,
                    location,
                ),
            Error::UnexpectedInput { location } =>
                write!(fmt, "Unexpected input at {}", location),
            Error::MissingClosingTags { inner: (ref tag_name, location), ref others } =>
                {
                    write!(fmt, "Unclosed '{}' tag starting at {}", tag_name, location)?;
                    if !others.is_empty() {
                        write!(fmt, "(also unclosed are: ")?;
                        for (index, &(ref tag_name, _)) in others.iter().enumerate() {
                            if index != 0 {
                                write!(fmt, ",")?;
                            }
                            write!(fmt, "'{}'", tag_name)?;
                        }
                        write!(fmt, ")")?;
                    }
                    Ok(())
                },
            Error::StackLimitExceeded { location, ref tag_name, limit } =>
                write!(fmt, "Stack limit ({}) exceeded by tag '{}' at {}",
                    limit,
                    tag_name,
                    location,
                ),
            Error::UnexpectedClosingTag { location, ref tag_name } =>
                write!(fmt, "Unexpected closing tag '{}' at {}", tag_name, location),
            Error::MismatchedClosingTag {
                ref opening_tag_name,
                ref closing_tag_name,
                opening_location,
                closing_location,
            } =>
                write!(fmt, "Closing tag '{}' at {} does not match opening tag '{}' at {}",
                    opening_tag_name,
                    opening_location,
                    closing_tag_name,
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
        let location::Located { value: event, location } = event;

        if let event::Event::OpeningTag { ref tag, .. } = event {
            if let Some(limit) = self.options.max_stack_depth {
                if limit == self.stack.len() {
                    self.finalize();
                    return Some(Err(Error::StackLimitExceeded {
                        location, 
                        tag_name: tag.to_string(),
                        limit,
                    }));
                }
            }
            self.stack.push(location.wrap(tag.clone()));
        }

        if let event::Event::ClosingTag { ref tag, .. } = event {
            match self.stack.pop() {
                None => {
                    self.finalize();
                    return Some(Err(Error::UnexpectedClosingTag {
                        location,
                        tag_name: tag.to_string(),
                    }));
                },
                Some(entry) =>
                    if *tag != entry.value {
                        self.finalize();
                        return Some(Err(Error::MismatchedClosingTag {
                            opening_location: entry.location,
                            opening_tag_name: entry.value.to_string(),
                            closing_location: location,
                            closing_tag_name: tag.to_string(),
                        }));
                    },
            }
        }

        self.rest = new_rest;
        self.mode = new_mode;

        Some(Ok(event))
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
        event: input.location().wrap(event::raw_data(content.into())),
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
        event: input.location().wrap(event::data(data.into())),
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
        event: input.location().wrap(event::comment(content.into())),
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
        event: input.location().wrap(event::doctype(content.into())),
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
            tag_name: (*identifier).into(),
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
            tag_name: tag.into(),
            attribute_name: (*name).into(),
        }),
    };

    let (value, rest) = match rest.take_until_char('"') {
        Some((value, rest)) => (value, rest.take_char('"').expect("end marker")),
        None => return Err(Error::UnclosedAttributeValue { 
            location: value_start,
            tag_name: tag.into(),
            attribute_name: (*name).into(),
        }),
    };

    Ok(Some(AttributeData {
        attribute: event::Attribute::new(name, Some(value.into())),
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
            tag_name: (*tag).into(),
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

