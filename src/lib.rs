#![warn(missing_docs)]

//! A stream based HTML5 templating system with code side logic.
//!
//! This library allows transforming a HTML5 stream with an iterator-like interface. It was
//! inspired by [`HTML::Zoom`](https://metacpan.org/release/HTML-Zoom).
//!
//! Since this library is intended for templating, the HTML5 parser is rather simple and
//! certainly lacking in some areas. Verification of input and output markup only happens
//! when necessary for transformations.
//!
//! # Templates
//!
//! Templates are sequences of events stored in a vector. The `transform` method of a template
//! can be used to generate a newly transformed template. See the `Api` type below for
//! information on available transform mechanisms.
//!
//! # Example
//! 
//! ```
//! # use std::error;
//! # fn example() -> Result<(), Box<error::Error>> {
//! use html5_fuser::{ Template, ParseOptions };
//!
//! let template = Template::from_str(r#"
//!     <html>
//!         <head><title>Template Title</title></head>
//!         <body>
//!             <ul id="items">
//!                 <li class="item"/>
//!             </ul>
//!         </body>
//!     </html>
//! "#, ParseOptions::default())?;
//!
//! let transformed = template.transform(|html| html
//!     .select("title", |html| html.replace_contents("New Title"))
//!     .select("#items", |html| html
//!         .repeat(2..5, |html, value| html
//!             .replace_contents(format!("Item {}", value))
//!         )
//!     )
//! )?;
//!
//! println!("{}", transformed);
//! # Ok(())
//! # }
//! # fn main() { example().unwrap() }
//! ```
//!
extern crate tendril;

#[cfg(test)]
macro_rules! test_transform {
    ($options:expr, $input:expr, $expected:expr, $transform:expr $(,)*) => {{
        let template = ::Template::from_str($input, $options).unwrap();
        let modified = template.transform($transform).unwrap();
        let rendered = format!("{}", &modified);
        assert_eq!(rendered, $expected);
        modified
    }}
}

#[cfg(test)]
macro_rules! test_stream_error {
    ($options:expr, $input:expr, $expected:pat, $transform:expr $(,)*) => {{
        let template = ::Template::from_str($input, $options).unwrap();
        let result = template.transform($transform);
        if let Err($expected) = result {
        } else {
            panic!("invalid result: {:?}", result);
        }
    }}
}

mod input;
pub mod content;
pub mod builder;
pub mod location;
pub mod template;
pub mod event;
pub mod text;
pub mod select;
pub mod transform;
pub mod modifier;
pub mod parse;

pub use event::{
    Stream,
    ElementStream,
    StreamError,
};

pub use template::{
    Template,
};

pub use parse::{
    Options as ParseOptions,
};

pub use transform::{
    Api,
    BoxedApi,
    BoxedElementApi,
};
