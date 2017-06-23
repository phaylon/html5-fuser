
//! Abstractions for text handling.
//!
//! These types are generally backed by storage inside of a `tendril`, and are optimized
//! to not copy `&'static str` values unless necessary.

mod base;
mod deferred;
mod identifier;
mod value;
mod data;
mod encoded;

use self::base::{ Text };
use self::deferred::{ Deferred };

pub(crate) use self::identifier::{ identifier_eq };
pub(crate) use self::identifier::{ validate as validate_identifier };
pub(crate) use self::encoded::{ EncodedText };

pub use self::encoded::{ EncodedStr };
pub use self::identifier::{ Identifier, IdentifierError, IntoIdentifier };
pub use self::value::{ Value, IntoValue };
pub use self::data::{ Data };

