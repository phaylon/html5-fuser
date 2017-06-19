
use std::rc;
use std::str;
use std::path;
use std::io;
use std::fs;
use std::fmt;
use std::error;

use event;
use parse;
use transform;

#[derive(Debug)]
pub enum InputError {
    LengthLimitExceeded {
        limit: usize,
    },
    Parse {
        error: parse::Error,
    },
}

impl error::Error for InputError {

    fn description(&self) -> &str { "Input error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            InputError::Parse { ref error } => Some(error),
            _ => None,
        }
    }
}

impl fmt::Display for InputError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            InputError::Parse { .. } =>
                write!(fmt, "Unable to parse input into HTML5 stream"),
            InputError::LengthLimitExceeded { limit } =>
                write!(fmt, "Input length limit ({}) was exceeded", limit),
        }
    }
}

#[derive(Debug)]
pub enum FileError {
    Open {
        path: path::PathBuf,
        error: io::Error,
    },
    Read {
        path: path::PathBuf,
        error: io::Error,
    },
    Input {
        path: path::PathBuf,
        error: InputError,
    },
    Decode {
        path: path::PathBuf,
        error: str::Utf8Error,
    },
}

impl error::Error for FileError {

    fn description(&self) -> &str { "File error" }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            FileError::Input { ref error, .. } => Some(error),
            FileError::Open { ref error, .. } => Some(error),
            FileError::Read { ref error, .. } => Some(error),
            FileError::Decode { ref error, .. } => Some(error),
        }
    }
}

impl fmt::Display for FileError {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let (action, path) = match *self {
            FileError::Input { ref path, .. } => ("process", path),
            FileError::Open { ref path, .. } => ("open", path),
            FileError::Read { ref path, .. } => ("read", path),
            FileError::Decode { ref path, .. } => ("decode", path),
        };
        write!(fmt, "Unable to {} file '{}'", action, path.display())
    }
}

#[derive(Debug)]
pub struct TemplateStream {
    events: rc::Rc<Vec<event::Event>>,
    index: usize,
}

impl event::Stream for TemplateStream {

    fn next_event(&mut self) -> event::StreamResult {
        match self.events.get(self.index) {
            Some(event) => {
                self.index += 1;
                Ok(Some(event.clone()))
            },
            None => Ok(None),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Template {
    events: rc::Rc<Vec<event::Event>>,
}

impl Template {

    pub fn from_file<P>(path: P, options: parse::Options) -> Result<Template, FileError>
    where P: AsRef<path::Path> {
        use std::io::{ BufRead };

        let path = path.as_ref();

        let mut buffer = fs::File::open(path)
            .map(io::BufReader::new)
            .map_err(|error| FileError::Open {
                path: path.into(),
                error,
            })?;

        let mut input = String::new();
        let limit = options
            .max_input_len
            .unwrap_or_else(usize::max_value);

        'read: loop {

            let consumed_len = {
                let data = buffer.fill_buf()
                    .map_err(|error| FileError::Read {
                        path: path.into(),
                        error,
                    })?;

                if data.is_empty() {
                    break 'read;
                }

                let valid_data = match str::from_utf8(data) {
                    Ok(valid_data) => valid_data,
                    Err(error) =>
                        if error.valid_up_to() == 0 {
                            return Err(FileError::Decode {
                                path: path.into(),
                                error,
                            });
                        } else {
                            str::from_utf8(&data[..error.valid_up_to()])
                                .expect("verified utf8")
                        },
                };
            
                input.push_str(valid_data);
                valid_data.len()
            };

            if input.len() > limit {
                return Err(FileError::Input {
                    path: path.into(),
                    error: InputError::LengthLimitExceeded { limit },
                });
            }

            buffer.consume(consumed_len);
        }

        Template::from_str(&input, options)
            .map_err(|error| FileError::Input {
                path: path.into(),
                error,
            })
    }

    pub fn from_str(input: &str, options: parse::Options) -> Result<Template, InputError> {

        if let Some(limit) = options.max_input_len {
            if input.len() > limit {
                return Err(InputError::LengthLimitExceeded {
                    limit,
                });
            }
        }

        Ok(Template {
            events: parse::Iter::new(input, options)
                .collect::<Result<_, _>>()
                .map(rc::Rc::new)
                .map_err(|error| InputError::Parse { error })?,
        })
    }

    pub(crate) fn to_stream(&self) -> TemplateStream {
        TemplateStream {
            events: self.events.clone(),
            index: 0,
        }
    }

    pub(crate) fn from_stream<S>(mut stream: S) -> Result<Template, event::StreamError>
    where S: event::Stream {
        let mut events = Vec::new();

        while let Some(event) = stream.next_event_skip_noop()? {
            events.push(event);
        }

        Ok(Template {
            events: rc::Rc::new(events),
        })
    }

    pub fn transform<B, R>(&self, builder: B) -> Result<Template, event::StreamError>
    where
        B: for<'t> FnOnce(transform::Api<'t, TemplateStream>) -> transform::Api<'t, R>,
        R: event::Stream,
    {
        let stream = transform::build_once(self.to_stream(), builder);
        Template::from_stream(stream)
    }
}

impl fmt::Display for Template {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        for event in self.events.iter() {
            fmt::Display::fmt(event, fmt)?;
        }
        Ok(())
    }
}

impl event::IntoStream for rc::Rc<Template> {

    type Stream = TemplateStream;

    fn into_stream(self) -> TemplateStream {
        self.to_stream()
    }
}

impl<'t> event::IntoStream for &'t Template {

    type Stream = TemplateStream;

    fn into_stream(self) -> TemplateStream {
        self.to_stream()
    }
}

impl event::IntoStream for Template {

    type Stream = TemplateStream;

    fn into_stream(self) -> TemplateStream {
        self.to_stream()
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };
    use std::rc;

    #[test]
    fn into_stream_ref() {
        let new = ::Template::from_str("<b></b>", Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a></a>",
            "<a><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_contents(&new)
                )
        );
    }

    #[test]
    fn into_stream_rc() {
        let new = rc::Rc::new(::Template::from_str("<b></b>", Default::default()).unwrap());
        test_transform!(
            Default::default(),
            "<a></a>",
            "<a><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_contents(new.clone())
                )
        );
    }

    #[test]
    fn into_stream() {
        test_transform!(
            Default::default(),
            "<a></a>",
            "<a><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .replace_contents(
                        ::Template::from_str("<b></b>", Default::default()).unwrap(),
                    )
                )
        );
    }

    #[test]
    fn roundtrip() {
        let original = "<a><b>foo</b></a>";
        test_transform!(Default::default(), original, original, |html| html);
        test_transform!(Default::default(), "", "", |html| html);
        test_transform!(Default::default(), "foo", "foo", |html| html);
    }
}
