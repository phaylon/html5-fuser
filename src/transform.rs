
use std::marker;

use event;
use modifier;
use select;
use template;
use text;

pub type BoxedApi<'t> = Api<'t, Box<event::Stream>>;

pub type BoxedElementApi<'t> = Api<'t, Box<event::ElementStream>>;

#[derive(Debug)]
pub struct Api<'t, S> {
    stream: S,
    api_lifetime_restriction: marker::PhantomData<&'t ()>,
}

pub(crate) fn apply<S, F, X>(stream: S, mut builder: F) -> X
where
    F: for<'tb> FnMut(Api<'tb, S>) -> Api<'tb, X>,
{
    builder(Api::pack(stream)).unpack()
}

pub(crate) fn apply_once<S, F, X>(stream: S, builder: F) -> X
where
    F: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, X>,
{
    builder(Api::pack(stream)).unpack()
}

impl<'t, S> Api<'t, S> {

    pub(crate) fn pack(stream: S) -> Api<'t, S> {
        Api {
            stream,
            api_lifetime_restriction: marker::PhantomData,
        }
    }

    pub(crate) fn unpack(self) -> S {
        self.stream
    }
}

type SelectAny<M, S, F, X> = modifier::Fallible<modifier::select::SelectAny<M, S, F, X>>;
type SelectDirect<M, S, F, X> = modifier::Fallible<modifier::select::SelectDirect<M, S, F, X>>;

impl<'t, S> Api<'t, S> where S: event::Stream {

    pub fn into_boxed(self) -> BoxedApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    pub fn select<M, F, X>(self, selector: M, transform: F)
    -> Api<'t, SelectAny<M::Selector, S, F, X>>
    where
        M: select::IntoSelector,
        F: for<'tb> FnMut(Api<'tb, modifier::select::Stream<S>>) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::Fallible::new(
            selector.into_selector()
                .map(move |selector| modifier::select::SelectAny::new(
                    selector,
                    self.stream,
                    transform,
                ))
                .map_err(Into::into)
        ))
    }

    pub fn select_direct<M, F, X>(self, selector: M, transform: F)
    -> Api<'t, SelectDirect<M::Selector, S, F, X>>
    where
        M: select::IntoSelector,
        F: for<'tb> FnMut(Api<'tb, modifier::select::Stream<S>>) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::Fallible::new(
            selector.into_selector()
                .map(move |selector| modifier::select::SelectDirect::new(
                    selector,
                    self.stream,
                    transform,
                ))
                .map_err(Into::into)
        ))
    }

    pub fn remove(self) -> Api<'t, modifier::remove::Remove<S>> {
        Api::pack(modifier::remove::Remove::new(self.stream))
    }

    pub fn append<N>(self, stream: N) -> Api<'t, modifier::append::Append<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::Append::new(self.stream, stream.into_stream()))
    }

    pub fn prepend<N>(self, stream: N) -> Api<'t, modifier::prepend::Prepend<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::Prepend::new(self.stream, stream.into_stream()))
    }

    pub fn replace<N>(self, stream: N) -> Api<'t, modifier::replace::Replace<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::Replace::new(self.stream, stream.into_stream()))
    }

    pub fn repeat<I, B, X>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::Repeat<S, I::IntoIter, B, X>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, modifier::repeat::Source>, I::Item) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::repeat::Repeat::new(iter.into_iter(), self.stream, builder))
    }

    pub fn apply_as_boxed<B, X>(self, builder: B) -> Api<'t, X>
    where
        B: for<'tb> FnOnce(BoxedApi<'tb>) -> Api<'tb, X>,
        X: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed())
    }

    pub fn apply<B, X>(self, builder: B) -> Api<'t, X>
    where
        B: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, X>,
        X: event::Stream,
    {
        builder(self)
    }

    pub fn apply_if<B, X>(self, predicate: bool, builder: B)
    -> Api<'t, modifier::apply::ApplyEither<X, S>>
    where
        B: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, X>,
        X: event::Stream,
    {
        if predicate {
            Api::pack(modifier::apply::ApplyEither::first(builder(self).unpack()))
        } else {
            Api::pack(modifier::apply::ApplyEither::second(self.stream))
        }
    }

    pub fn apply_if_else<B1, X1, B2, X2>(self, predicate: bool, if_builder: B1, else_builder: B2)
    -> Api<'t, modifier::apply::ApplyEither<X1, X2>>
    where
        B1: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, X1>,
        X1: event::Stream,
        B2: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, X2>,
        X2: event::Stream,
    {
        if predicate {
            Api::pack(modifier::apply::ApplyEither::first(if_builder(self).unpack()))
        } else {
            Api::pack(modifier::apply::ApplyEither::second(else_builder(self).unpack()))
        }
    }

    pub fn apply_as_template<B>(self, builder: B)
    -> Api<'t, modifier::apply::TemplateApply<S, B>>
    where
        B: FnOnce(template::Template) -> Result<template::Template, event::StreamError>,
    {
        Api::pack(modifier::apply::TemplateApply::new(self.stream, builder))
    }
}

type SubSelectAny<M, S, F, X> =
    modifier::Fallible<modifier::select::SubSelectAny<M, S, F, X>>;

type SubSelectDirect<M, S, F, X> =
    modifier::Fallible<modifier::select::SubSelectDirect<M, S, F, X>>;

type SubSelectInput<S> = modifier::select::Stream<modifier::select::ContentStream<S>>;

impl<'t, S> Api<'t, S> where S: event::ElementStream {

    pub fn into_boxed_element(self) -> BoxedElementApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    pub fn subselect<M, F, X>(self, selector: M, transform: F)
    -> Api<'t, SubSelectAny<M::Selector, S, F, X>>
    where
        M: select::IntoSelector,
        F: for<'tb> FnMut(Api<'tb, SubSelectInput<S>>) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::Fallible::new(
            selector.into_selector()
                .map(move |selector| modifier::select::SubSelectAny::new(
                    selector,
                    self.stream,
                    transform,
                ))
                .map_err(Into::into)
        ))
    }

    pub fn subselect_direct<M, F, X>(self, selector: M, transform: F)
    -> Api<'t, SubSelectDirect<M::Selector, S, F, X>>
    where
        M: select::IntoSelector,
        F: for<'tb> FnMut(Api<'tb, SubSelectInput<S>>) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::Fallible::new(
            selector.into_selector()
                .map(move |selector| modifier::select::SubSelectDirect::new(
                    selector,
                    self.stream,
                    transform,
                ))
                .map_err(Into::into)
        ))
    }

    pub fn subselect_contents<F, X>(self, builder: F)
    -> Api<'t, modifier::select::SelectContent<S, F, X>>
    where
        F: for<'tb> FnOnce(Api<'tb, modifier::select::ContentStream<S>>) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::select::SelectContent::new(self.stream, builder))
    }

    pub fn remove_contents(self) -> Api<'t, modifier::remove::RemoveContents<S>> {
        Api::pack(modifier::remove::RemoveContents::new(self.stream))
    }

    pub fn append_contents<N>(self, stream: N)
    -> Api<'t, modifier::append::AppendContents<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::AppendContents::new(self.stream, stream.into_stream()))
    }

    pub fn prepend_contents<N>(self, stream: N)
    -> Api<'t, modifier::prepend::PrependContents<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::PrependContents::new(self.stream, stream.into_stream()))
    }

    pub fn replace_contents<N>(self, stream: N)
    -> Api<'t, modifier::replace::ReplaceContents<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::ReplaceContents::new(self.stream, stream.into_stream()))
    }

    pub fn repeat_contents<I, B, X>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::RepeatContents<S, I::IntoIter, B, X>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, modifier::repeat::Source>, I::Item) -> Api<'tb, X>,
        X: event::Stream,
    {
        Api::pack(modifier::repeat::RepeatContents::new(iter.into_iter(), self.stream, builder))
    }

    pub fn add_attribute<N, V>(self, name: N, value: V)
    -> Api<'t, modifier::Fallible<modifier::attribute::AddAttribute<S>>>
    where
        N: text::IntoIdentifier,
        V: text::IntoValue,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::AddAttribute::new(
                    self.stream,
                    name,
                    Some(value.into_value()),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn add_empty_attribute<N>(self, name: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::AddAttribute<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::AddAttribute::new(self.stream, name, None))
                .map_err(Into::into)
        ))
    }

    pub fn add_to_attribute<N, V, J>(self, name: N, value: V, separator: J)
    -> Api<'t, modifier::Fallible<modifier::attribute::AddToAttribute<S>>>
    where
        N: text::IntoIdentifier,
        V: text::IntoValue,
        J: text::IntoValue,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::AddToAttribute::new(
                    self.stream,
                    name,
                    value.into_value(),
                    separator.into_value(),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn replace_attribute<N, V>(self, name: N, value: V)
    -> Api<'t, modifier::Fallible<modifier::attribute::ReplaceAttribute<S>>>
    where
        N: text::IntoIdentifier,
        V: text::IntoValue,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::ReplaceAttribute::new(
                    self.stream,
                    name,
                    Some(value.into_value()),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn set_attribute<N, V>(self, name: N, value: V)
    -> Api<'t, modifier::Fallible<modifier::attribute::SetAttribute<S>>>
    where
        N: text::IntoIdentifier,
        V: text::IntoValue,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::SetAttribute::new(
                    self.stream,
                    name,
                    Some(value.into_value()),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn set_empty_attribute<N>(self, name: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::SetAttribute<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::SetAttribute::new(self.stream, name, None))
                .map_err(Into::into)
        ))
    }

    pub fn remove_attribute<N>(self, name: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::RemoveAttribute<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::RemoveAttribute::new(self.stream, name))
                .map_err(Into::into)
        ))
    }

    pub fn remove_class<N>(self, name: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::RemoveClass<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::RemoveClass::new(self.stream, name))
                .map_err(Into::into)
        ))
    }

    pub fn remove_id(self)
    -> Api<'t, modifier::attribute::RemoveAttribute<S>> {
        Api::pack(modifier::attribute::RemoveAttribute::new(
            self.stream,
            text::Identifier::from_static_str("id").expect("remove_id attribute identifier"),
        ))
    }

    pub fn set_id<N>(self, id: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::SetAttribute<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            id.into_identifier()
                .map(move |id| modifier::attribute::SetAttribute::new(
                    self.stream,
                    text::Identifier::from_static_str("id")
                        .expect("remove_id attribute identifier"),
                    Some(id.into_value()),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn add_class<N>(self, name: N)
    -> Api<'t, modifier::Fallible<modifier::attribute::AddToAttribute<S>>>
    where
        N: text::IntoIdentifier,
    {
        Api::pack(modifier::Fallible::new(
            name.into_identifier()
                .map(move |name| modifier::attribute::AddToAttribute::new(
                    self.stream,
                    text::Identifier::from_static_str("class")
                        .expect("add_class attribute identifier"),
                    name.into_value(),
                    text::Value::from_unencoded_static_str(" "),
                ))
                .map_err(Into::into)
        ))
    }

    pub fn apply_as_boxed_element<B, X>(self, builder: B) -> Api<'t, X>
    where
        B: for<'tb> FnOnce(BoxedElementApi<'tb>) -> Api<'tb, X>,
        X: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed_element())
    }
}

pub(crate) trait BuildOnce<S> {

    type Stream: event::Stream;

    fn build_once(self, stream: S) -> Self::Stream;
}

impl<F, S, R> BuildOnce<S> for F
where
    S: event::Stream,
    F: for<'t> FnOnce(Api<'t, S>) -> Api<'t, R>,
    R: event::Stream,
{
    type Stream = R;

    fn build_once(self, stream: S) -> R {
        apply_once(stream, self)
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn apply_as_boxed_element() {

        fn map_boxed_elem(api: ::BoxedElementApi) -> ::BoxedElementApi {
            api.remove_contents().into_boxed_element()
        }

        test_transform!(
            Default::default(),
            "<a>01<b>23</b>45</a>",
            "<a>01<b></b>45</a>",
            |html| html
                .select(::select::Tag::from_str("b"), |html|
                    html.apply_as_boxed_element(map_boxed_elem)
                )
        );
    }

    #[test]
    fn into_boxed_element() {

        fn map_boxed_elem(api: ::BoxedElementApi) -> ::BoxedElementApi {
            api.remove_contents().into_boxed_element()
        }

        test_transform!(
            Default::default(),
            "<a>01<b>23</b>45</a>",
            "<a>01<b></b>45</a>",
            |html| html
                .select(::select::Tag::from_str("b"), |html|
                    map_boxed_elem(html.into_boxed_element())
                )
        );
    }

    #[test]
    fn apply_as_boxed() {

        fn map_boxed(api: ::BoxedApi) -> ::BoxedApi {
            api.remove().into_boxed()
        }

        test_transform!(
            Default::default(),
            "<a>01<b>23</b>45</a>",
            "<a>0145</a>",
            |html| html
                .select(::select::Tag::from_str("b"), |html|
                    html.apply_as_boxed(map_boxed)
                )
        );
    }

    #[test]
    fn into_boxed() {

        fn map_boxed(api: ::BoxedApi) -> ::BoxedApi {
            api.remove().into_boxed()
        }

        test_transform!(
            Default::default(),
            "<a>01<b>23</b>45</a>",
            "<a>0145</a>",
            |html| html
                .select(::select::Tag::from_str("b"), |html|
                    map_boxed(html.into_boxed())
                )
        );
    }
}
