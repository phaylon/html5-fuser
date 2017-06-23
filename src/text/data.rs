
use std::str;
use std::fmt;

use tendril;

use text;

/// An encoded piece of content.
///
/// The content is stored inside a `tendril` if dynamic storage is required. Cloning them is
/// relatively cheap.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions };
/// use html5_fuser::text::{ Data };
///
/// let new_data = Data::from_unencoded_str("new data");
///
/// let template = Template::from_str(r#"
///     <div id="content"></div>
/// "#, ParseOptions::default())?;
///
/// let output = format!("{}", template.transform(|html| html
///     .select("div", |html| html
///         .replace_contents(&new_data)
///     )
/// )?);
///
/// assert!(output.contains(r#"<div id="content">new data</div>"#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Data {
    text: text::EncodedText,
}

impl fmt::Display for Data {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        fmt::Display::fmt(&self.text, fmt)
    }
}

impl From<text::Value> for Data {

    fn from(text: text::Value) -> Data {
        Data { text: text.into_encoded_text() }
    }
}

impl Data {

    pub(crate) fn from_encoded_tendril(value: tendril::StrTendril) -> Data {
        Data {
            text: text::EncodedText::from_encoded_tendril(value),
        }
    }

    pub(crate) fn from_raw(value: &str) -> Data {
        Data {
            text: text::EncodedText::from_raw(value),
        }
    }

    /// Construct by encoding the provided value.
    pub fn from_unencoded_str<T>(value: T) -> Data where T: AsRef<str> {
        Data {
            text: text::EncodedText::from_unencoded_str(value.as_ref()),
        }
    }

    /// Construct by providing the provided static value.
    ///
    /// With this method a copy of the actual data is only required when encoding is
    /// necessary.
    pub fn from_unencoded_static_str(value: &'static str) -> Data {
        Data {
            text: text::EncodedText::from_unencoded_static_str(value),
        }
    }

    /// Gives an encoded string slice.
    pub fn as_encoded_ref(&self) -> text::EncodedStr {
        self.text.as_encoded_ref()
    }
}

