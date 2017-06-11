
use location;
use text;

#[derive(Debug, Copy, Clone)]
pub struct Input<'i> {
    content: &'i str,
    location: location::Location,
}

type Split<'i, T> = (T, Input<'i>);
type StrSplit<'i> = Split<'i, &'i str>;

impl<'i> Input<'i> {

    pub fn new(content: &'i str) -> Input<'i> {
        Input {
            content: content,
            location: location::Location::at_start(),
        }
    }

    pub fn is_empty(&self) -> bool { self.content.is_empty() }

    pub fn end(&self) -> Input<'i> {
        self.advanced(self.content.len())
    }

    pub fn location(&self) -> location::Location { self.location }

    fn consumed(&self, len: usize) -> StrSplit<'i> {
        (&self.content[..len], self.advanced(len))
    }

    fn consume_rest(&self) -> StrSplit<'i> {
        self.consumed(self.content.len())
    }

    fn advanced(&self, len: usize) -> Input<'i> {
        Input {
            content: &self.content[len..],
            location: self.location.with_consumed(&self.content[..len]),
        }
    }
    pub fn take_all(&self) -> StrSplit<'i> {
        self.consume_rest()
    }

    pub fn take_until_str(&self, end: &str) -> Option<StrSplit<'i>> {
        match self.content.find(end) {
            None => None,
            Some(len) => Some(self.consumed(len)),
        }
    }

    pub fn take_until_char(&self, end: char) -> Option<StrSplit<'i>> {
        match self.content.find(end) {
            None => None,
            Some(len) => Some(self.consumed(len)),
        }
    }

    pub fn take_until_end_char<F>(&self, chr: char, check: F) -> Option<StrSplit<'i>>
    where F: Fn(Input<'i>) -> bool {
        for (index, _) in self.content.match_indices(chr) {
            let rest = self.advanced(index);
            if check(rest) {
                return Some(self.consumed(index));
            }
        }
        None
    }

    pub fn take_str(&self, pat: &str) -> Option<Input<'i>> {
        if self.content.starts_with(pat) {
            let len = pat.len();
            Some(self.advanced(len))
        } else {
            None
        }
    }

    pub fn take_char(&self, chr: char) -> Option<Input<'i>> {
        if self.content.starts_with(chr) {
            let len = chr.len_utf8();
            Some(self.advanced(len))
        } else {
            None
        }
    }

    pub fn skip_whitespace(&self) -> Input<'i> {
        let trimmed = self.content.trim_left();
        let len = self.content.len() - trimmed.len();
        self.advanced(len)
    }

    pub fn take_word(&self) -> Option<StrSplit<'i>> {

        const TERMINATORS: &'static [char] = &[
            ' ', '\t', '\r', '\n', '>', '<', '"', '/', '=', '\'', '&',
        ];

        let len = match self.content.find(TERMINATORS) {
            Some(pos) => pos,
            None => self.content.len(),
        };

        if len == 0 {
            None
        } else {
            Some(self.consumed(len))
        }
    }

    pub fn try_take_exact_identifier(&self, identifier: &str)
    -> Option<Split<'i, text::Identifier>> {
        match self.take_word() {
            None => None,
            Some((found, rest)) => match found.parse::<text::Identifier>() {
                Ok(found_identifier) =>
                    if text::identifier_eq(&found_identifier, identifier) {
                        Some((found_identifier, rest))
                    } else {
                        None
                    },
                Err(_) => None,
            },
        }
    }

    pub fn take_identifier(&self)
    -> Result<Option<Split<'i, text::Identifier>>, text::IdentifierError> {
        match self.take_word() {
            None => Ok(None),
            Some((word, rest)) => match word.parse() {
                Ok(identifier) => Ok(Some((identifier, rest))),
                Err(error) => Err(error),
            },
        }
    }
}
