
extern crate tendril;

#[cfg(test)]
macro_rules! test_transform {
    ($options:expr, $input:expr, $expected:expr, $transform:expr) => {{
        let template = ::Template::from_str($input, $options).unwrap();
        let modified = template.transform($transform).unwrap();
        let rendered = format!("{}", &modified);
        assert_eq!(rendered, $expected);
        modified
    }}
}

mod input;

pub mod content;
pub mod location;
pub mod template;
pub mod event;
pub mod text;
pub mod select;
pub mod transform;
pub mod modifier;
pub mod parse;

pub use template::{
    Template,
};

pub use parse::{
    Options as ParseOptions,
};
