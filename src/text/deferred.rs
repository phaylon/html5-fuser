
use std::ops;
use std::str;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Deferred<T> {
    StaticStr(&'static str),
    Actual(T),
}

impl<T> Deferred<T> where T: From<&'static str> {

    pub(crate) fn into_actual(self) -> T {
        match self {
            Deferred::StaticStr(value) => value.into(),
            Deferred::Actual(value) => value,
        }
    }

    pub(crate) fn actual(&mut self) -> &mut T {
        if let Deferred::StaticStr(value) = *self {
            *self = Deferred::Actual(value.into());
        }
        match *self {
            Deferred::StaticStr(_) => panic!("unexpected static storage"),
            Deferred::Actual(ref mut value) => value,
        }
    }
}

impl<T> fmt::Display for Deferred<T> where T: fmt::Display {

    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Deferred::StaticStr(value) => fmt::Display::fmt(value, fmt),
            Deferred::Actual(ref value) => fmt::Display::fmt(value, fmt),
        }
    }
}

impl<T> ops::Deref for Deferred<T> where T: ops::Deref<Target=str> {

    type Target = str;

    fn deref(&self) -> &str {
        match *self {
            Deferred::StaticStr(value) => value,
            Deferred::Actual(ref value) => value,
        }
    }
}

