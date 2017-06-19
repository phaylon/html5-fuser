
//! Abstractions for parameterized transform tree construction.
//!
//! These traits are intended for internal reuse of modifiers. You shouldn't have to
//! interact with them directly.

use event;
use transform;

/// Reusable builder allowing mutation with an additional argument.
pub trait BuildMutMapped<S, T> {

    type Stream: event::Stream;

    fn build_mut_mapped<'t>(&mut self, stream: transform::Api<'t, S>, value: T)
    -> transform::Api<'t, Self::Stream>;
}

impl<S, T, F, R> BuildMutMapped<S, T> for F
where
    S: event::Stream,
    F: for<'tb> FnMut(transform::Api<'tb, S>, T) -> transform::Api<'tb, R>,
    R: event::Stream,
{
    type Stream = R;

    fn build_mut_mapped<'t>(&mut self, stream: transform::Api<'t, S>, value: T)
    -> transform::Api<'t, Self::Stream> {
        self(stream, value)
    }
}

/// Reusable builder allowing mutation.
pub trait BuildMut<S> {

    type Stream: event::Stream;

    fn build_mut<'t>(&mut self, stream: transform::Api<'t, S>)
    -> transform::Api<'t, Self::Stream>;
}

impl<S, F, R> BuildMut<S> for F
where
    S: event::Stream,
    F: for<'tb> FnMut(transform::Api<'tb, S>) -> transform::Api<'tb, R>,
    R: event::Stream,
{
    type Stream = R;

    fn build_mut<'t>(&mut self, stream: transform::Api<'t, S>)
    -> transform::Api<'t, Self::Stream> {
        self(stream)
    }
}

/// A builder that can only be used once.
pub trait BuildOnce<S> {

    type Stream: event::Stream;

    fn build_once(self, stream: transform::Api<S>)
    -> transform::Api<Self::Stream>;
}

impl<S, F, R> BuildOnce<S> for F
where
    F: for<'tb> FnOnce(transform::Api<'tb, S>) -> transform::Api<'tb, R>,
    R: event::Stream,
{
    type Stream = R;

    fn build_once(self, stream: transform::Api<S>)
    -> transform::Api<Self::Stream> {
        self(stream)
    }
}

