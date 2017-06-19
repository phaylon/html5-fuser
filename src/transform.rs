
use std::marker;

use event;
use modifier;
use select;
use template;
use text;
use builder;

pub(crate) fn build_once<S, B>(stream: S, builder: B) -> B::Stream
where
    S: event::Stream,
    B: builder::BuildOnce<S>,
{
    Api::unpack(builder.build_once(Api::pack(stream)))
}

pub(crate) fn build_mut<S, B>(stream: S, builder: &mut B) -> B::Stream
where
    S: event::Stream,
    B: builder::BuildMut<S>,
{
    Api::unpack(builder.build_mut(Api::pack(stream)))
}

pub(crate) fn build_mut_mapped<S, B, T>(stream: S, builder: &mut B, value: T) -> B::Stream
where
    S: event::Stream,
    B: builder::BuildMutMapped<S, T>,
{
    Api::unpack(builder.build_mut_mapped(Api::pack(stream), value))
}

pub type BoxedApi<'t> = Api<'t, Box<event::Stream>>;
pub type BoxedElementApi<'t> = Api<'t, Box<event::ElementStream>>;

#[derive(Debug)]
pub struct Api<'t, S> {
    stream: S,
    api_lifetime_restriction: marker::PhantomData<&'t ()>,
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

type SelectAny<S, M, B> = modifier::Fallible<modifier::select::SelectAny<S, M, B>>;
type SelectDirect<S, M, B> = modifier::Fallible<modifier::select::SelectDirect<S, M, B>>;

impl<'t, S> Api<'t, S> where S: event::Stream {

    pub fn into_boxed(self) -> BoxedApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    pub fn select<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SelectAny<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnMut(Api<'tb, modifier::select::BuildElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SelectAny::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    pub fn select_direct<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SelectDirect<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnMut(Api<'tb, modifier::select::BuildElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SelectDirect::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    pub fn remove(self) -> Api<'t, modifier::remove::Remove<S>> {
        Api::pack(modifier::remove::Remove::new(self.stream))
    }

    pub fn prepend<N>(self, stream: N) -> Api<'t, modifier::prepend::Prepend<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::Prepend::new(self.stream, stream.into_stream()))
    }

    pub fn append<N>(self, stream: N) -> Api<'t, modifier::append::Append<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::Append::new(self.stream, stream.into_stream()))
    }

    pub fn replace<N>(self, stream: N) -> Api<'t, modifier::replace::Replace<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::Replace::new(self.stream, stream.into_stream()))
    }

    pub fn repeat<I, B, R>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::Repeat<S, I::IntoIter, B>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, modifier::repeat::ElementTemplate<S>>, I::Item)
            -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::repeat::Repeat::new(self.stream, iter.into_iter(), builder))
    }

    pub fn apply<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, R>,
        R: event::Stream,
    {
        builder(self)
    }

    pub fn apply_if<B, R>(self, predicate: bool, builder: B)
    -> Api<'t, modifier::apply::ApplyEither<R, S>>
    where
        B: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, R>,
        R: event::Stream,
    {
        if predicate {
            Api::pack(modifier::apply::ApplyEither::first(builder(self).unpack()))
        } else {
            Api::pack(modifier::apply::ApplyEither::second(self.stream))
        }
    }

    pub fn apply_if_else<B1, B2, R1, R2>(self, predicate: bool, if_builder: B1, else_builder: B2)
    -> Api<'t, modifier::apply::ApplyEither<R1, R2>>
    where
        B1: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, R1>,
        R1: event::Stream,
        B2: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, R2>,
        R2: event::Stream,
    {
        if predicate {
            Api::pack(modifier::apply::ApplyEither::first(
                if_builder(self).unpack()
            ))
        } else {
            Api::pack(modifier::apply::ApplyEither::second(
                else_builder(self).unpack()
            ))
        }
    }

    pub fn apply_as_boxed<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(BoxedApi<'tb>) -> Api<'tb, R>,
        R: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed())
    }

    pub fn apply_as_template<B>(self, builder: B)
    -> Api<'t, modifier::apply::TemplateApply<S, B>>
    where
        B: FnOnce(template::Template) -> Result<template::Template, event::StreamError>,
    {
        Api::pack(modifier::apply::TemplateApply::new(self.stream, builder))
    }
}

type SubSelectAny<S, M, B> = modifier::Fallible<modifier::select::SubSelectAny<S, M, B>>;
type SubSelectDirect<S, M, B> = modifier::Fallible<modifier::select::SubSelectDirect<S, M, B>>;

impl<'t, S> Api<'t, S> where S: event::ElementStream {

    pub fn into_boxed_element(self) -> BoxedElementApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    pub fn subselect_contents<B, R>(self, builder: B)
    -> Api<'t, modifier::select::SelectContent<S, B>>
    where
        B: for<'tb> FnOnce(Api<'tb, modifier::select::CurrentContent<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::select::SelectContent::new(self.stream, builder))
    }

    pub fn subselect<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SubSelectAny<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnMut(Api<'tb, modifier::select::BuildSubElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SubSelectAny::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    pub fn subselect_direct<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SubSelectDirect<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnMut(Api<'tb, modifier::select::BuildSubElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SubSelectDirect::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    pub fn remove_contents(self) -> Api<'t, modifier::remove::RemoveContent<S>> {
        Api::pack(modifier::remove::RemoveContent::new(self.stream))
    }

    pub fn prepend_contents<N>(self, stream: N)
    -> Api<'t, modifier::prepend::PrependContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::PrependContent::new(self.stream, stream.into_stream()))
    }

    pub fn append_contents<N>(self, stream: N)
    -> Api<'t, modifier::append::AppendContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::AppendContent::new(self.stream, stream.into_stream()))
    }

    pub fn replace_contents<N>(self, stream: N)
    -> Api<'t, modifier::replace::ReplaceContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::ReplaceContent::new(self.stream, stream.into_stream()))
    }

    pub fn repeat_contents<I, B, R>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::RepeatContent<S, I::IntoIter, B>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, template::TemplateStream>, I::Item) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::repeat::RepeatContent::new(self.stream, iter.into_iter(), builder))
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

    pub fn apply_as_boxed_element<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(BoxedElementApi<'tb>) -> Api<'tb, R>,
        R: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed_element())
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
