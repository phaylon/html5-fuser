
use std::fmt;

#[derive(Debug, Clone)]
pub(crate) struct Located<T> {
    pub location: Location,
    pub value: T,
}

/// A source location.
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location {
    line: usize,
    column: usize,
    offset: usize,
}

impl Location {

    pub(crate) fn new(line: usize, column: usize, offset: usize) -> Location {
        Location { line, column, offset }
    }

    pub(crate) fn at_start() -> Location {
        Location::new(1, 1, 0)
    }

    pub(crate) fn with_consumed(&self, parsed: &str) -> Location {

        let nl_count = parsed
            .chars()
            .filter(|c| *c == '\n')
            .count();
        let last_line = parsed
            .rfind('\n')
            .map(|nl_pos| &parsed[(nl_pos+1)..])
            .unwrap_or(parsed);
        let last_line_chars = last_line
            .chars()
            .count();

        Location {
            offset: self.offset + parsed.len(),
            line: self.line + nl_count,
            column: last_line_chars + if nl_count > 0 { 1 } else { self.column },
        }
    }

    pub(crate) fn wrap<T>(&self, value: T) -> Located<T> {
        Located {
            location: *self,
            value,
        }
    }

    /// The line number (starting at 1).
    pub fn line(&self) -> usize { self.line }

    /// The column number (starting at 1).
    pub fn column(&self) -> usize { self.column }

    /// The byte offset (starting at 0).
    pub fn offset(&self) -> usize { self.offset }
}

impl fmt::Display for Location {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt,
            "line {} column {} (byte offset {})",
            self.line,
            self.column,
            self.offset,
        )
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn advance() {
        let location = super::Location::at_start();
        assert_eq!(location.with_consumed(""), super::Location::at_start());
        assert_eq!(location.with_consumed("foo"), super::Location::new(1, 4, 3));
        assert_eq!(location.with_consumed("foo \n bar"), super::Location::new(2, 5, 9));
    }
}
