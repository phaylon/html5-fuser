
//! Transformation API.

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

/// Convenience type to make transforming via functions easier.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions, BoxedApi };
/// use html5_fuser::text::{ Data };
///
/// fn transform_head<'a>(html: BoxedApi<'a>, title: &str) -> BoxedApi<'a> {
///     let title = Data::from_unencoded_str(title);
///     html.select("title",
///         move |html| html.replace_contents(title.clone())
///     )
///     .into_boxed()
/// }
///
/// let template = Template::from_str(r#"
///     <html>
///         <head><title>Title</title></head>
///         <body></body>
///     </html>
/// "#, ParseOptions::default())?;
///
/// let output = format!("{}", template.transform(|html| html
///     .select("head", |html| html
///         .apply_as_boxed(|html| transform_head(html, "New Title"))
///     )
/// )?);
///
/// assert!(output.contains(r#"<title>New Title</title>"#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
pub type BoxedApi<'t> = Api<'t, Box<event::Stream>>;

/// Convenience type to make transforming elements via functions easier.
///
/// # Examples
///
/// ```
/// # use std::error;
/// # fn run() -> Result<(), Box<error::Error>> {
/// use html5_fuser::{ Template, ParseOptions, BoxedElementApi };
/// use html5_fuser::text::{ Data };
///
/// fn transform_title<'a>(
///     html: BoxedElementApi<'a>,
///     title: &str,
/// ) -> BoxedElementApi<'a> {
///     let title = Data::from_unencoded_str(title);
///     html.replace_contents(title)
///         .into_boxed_element()
/// }
///
/// let template = Template::from_str(r#"
///     <html>
///         <head><title>Title</title></head>
///         <body></body>
///     </html>
/// "#, ParseOptions::default())?;
///
/// let output = format!("{}", template.transform(|html| html
///     .select("title", |html| html
///         .apply_as_boxed_element(|html|
///             transform_title(html, "New Title")
///         )
///     )
/// )?);
///
/// assert!(output.contains(r#"<title>New Title</title>"#));
/// # Ok(()) }
/// # fn main() { run().unwrap() }
/// ```
pub type BoxedElementApi<'t> = Api<'t, Box<event::ElementStream>>;

/// Transform API wrapper type.
///
/// This type is wrapped around all transformable streams and provides the actual user-facing
/// transformation API.
///
/// The API is split into two parts. A general implementation available to all streams that
/// assumes a stream can contain multiple elements, and one specific to `ElementStream`s that
/// allows special single-element transformations, like adjusting attributes and contents.
///
/// A stream is considered element specific until a transformation is performed that has a
/// non-element result, like `remove` or `repeat`.
///
/// In addition to encapsulation, the API will also track its lifetime to ensure streams aren't
/// permanently stored.
///
/// It should be noted that all these methods build a tree of transformers. The transformation
/// itself only happens when the full transform is constructed and returned from the closure
/// given to `Template::transform`
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

type SelectAnyOnce<S, M, B> = modifier::Fallible<modifier::select::SelectAnyOnce<S, M, B>>;
type SelectDirectOnce<S, M, B> = modifier::Fallible<modifier::select::SelectDirectOnce<S, M, B>>;

/// This is the general API implementation that applies to all kinds of streams.
impl<'t, S> Api<'t, S> where S: event::Stream {

    /// Turn the `Api` into a `BoxedApi`.
    ///
    /// This is useful as return value from transforming functions.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions, BoxedApi };
    /// use html5_fuser::text::{ Data };
    ///
    /// fn transform_head<'a>(html: BoxedApi<'a>, title: &str) -> BoxedApi<'a> {
    ///     let title = Data::from_unencoded_str(title);
    ///     html.select("title",
    ///         move |html| html.replace_contents(title.clone())
    ///     )
    ///     .into_boxed()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("head", |html| html
    ///         .apply_as_boxed(|html| transform_head(html, "New Title"))
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn into_boxed(self) -> BoxedApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    /// Perform the given transformation on the stream.
    ///
    /// This provides a way to run a closure or function to construct the transformation
    /// in the mid of building a stream without intermediate variables.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{
    ///     Template, ParseOptions, BoxedApi, Api, ElementStream,
    /// };
    ///
    /// // Returns a boxed Api stream for convenience
    /// fn add_contents<S>(html: Api<S>) -> BoxedApi
    /// where S: ElementStream + 'static {
    ///     html.replace_contents("Fn Content")
    ///         .into_boxed()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <div id="one"/>
    ///     <div id="two"/>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     // Transform via function:
    ///     .select("#one", |html| html
    ///         .add_class("first")
    ///         .apply(add_contents)
    ///     )
    ///     // Transform via closure:
    ///     .select("#two", |html| html
    ///         .add_class("second")
    ///         .apply(|html| html.replace_contents("Closure Content"))
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div id="one" class="first">Fn Content</div>"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<div id="two" class="second">Closure Content</div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn apply<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(Api<'tb, S>) -> Api<'tb, R>,
        R: event::Stream,
    {
        builder(self)
    }

    /// Apply the transformation only if a condition holds true.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{
    ///     Template, ParseOptions, BoxedApi, BoxedElementApi,
    /// };
    ///
    /// fn set_value(html: BoxedElementApi, value: i32) -> BoxedApi {
    ///     html.apply_if(
    ///         value >= 0,
    ///         move |html| html.replace_contents(value),
    ///     ).into_boxed()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <div id="one">Default Value</div>
    ///     <div id="two">Default Value</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("#one", |html| set_value(html.into_boxed_element(), 10))
    ///     .select("#two", |html| set_value(html.into_boxed_element(), -10))
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div id="one">10</div>"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<div id="two">Default Value</div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Apply a transformation when a condition holds, and a different tranformation otherwise.
    ///
    /// This transformation is like `apply_if`, but you can also supply a transformation if the
    /// condition isn't true.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{
    ///     Template, ParseOptions, BoxedApi, BoxedElementApi,
    /// };
    ///
    /// fn fill_items(html: BoxedElementApi, items: Vec<i32>) -> BoxedApi {
    ///     html.apply_if_else(
    ///         items.is_empty(),
    ///         |html| html
    ///             .subselect("li.item", |html| html.remove()),
    ///         move |html| html
    ///             .subselect("li.none", |html| html.remove())
    ///             .subselect("li.item", move |html| html
    ///                 .repeat(items.clone(), |html, value| html
    ///                     .append_contents(" ")
    ///                     .append_contents(value)
    ///                 )
    ///             ),
    ///     ).into_boxed()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <ul>
    ///         <li class="item">Item</li>
    ///         <li class="none">None</li>
    ///     </ul>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("ul", |html|
    ///         fill_items(html.into_boxed_element(), vec![2, 3])
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<li class="item">Item 2</li>"#));
    /// assert!(output.contains(r#"<li class="item">Item 3</li>"#));
    /// assert!(!output.contains(r#"<li class="none">"#));
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("ul", |html|
    ///         fill_items(html.into_boxed_element(), vec![])
    ///     )
    /// )?);
    ///
    /// assert!(!output.contains(r#"<li class="item">"#));
    /// assert!(output.contains(r#"<li class="none">None</li>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Apply transformation to a boxed version of the stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions, BoxedApi };
    /// use html5_fuser::text::{ Data };
    ///
    /// fn transform_head<'a>(html: BoxedApi<'a>, title: &str) -> BoxedApi<'a> {
    ///     let title = Data::from_unencoded_str(title);
    ///     html.select_once("title",
    ///         move |html| html.replace_contents(title)
    ///     )
    ///     .into_boxed()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("head", |html| html
    ///         .apply_as_boxed(|html| transform_head(html, "New Title"))
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn apply_as_boxed<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(BoxedApi<'tb>) -> Api<'tb, R>,
        R: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed())
    }

    /// Collect a template to be transformed and reinserted.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions, StreamError };
    /// use html5_fuser::text::{ Data };
    ///
    /// fn transform_head(
    ///     template: Template,
    ///     title: &str,
    /// ) -> Result<Template, StreamError> {
    ///     let title = Data::from_unencoded_str(title);
    ///     template.transform(|html| html
    ///         .select_once("title",
    ///             |html| html.replace_contents(title)
    ///         )
    ///     )
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("head", |html| html
    ///         .apply_as_template(|template|
    ///             transform_head(template, "New Title")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn apply_as_template<B>(self, builder: B)
    -> Api<'t, modifier::apply::TemplateApply<S, B>>
    where
        B: FnOnce(template::Template) -> Result<template::Template, event::StreamError>,
    {
        Api::pack(modifier::apply::TemplateApply::new(self.stream, builder))
    }

    /// Apply transformations to each matching element in a stream.
    ///
    /// This function will match elements at any depth, but the transformation is not
    /// recursive. The stream provided to the transformation closure will be an element stream.
    /// The resulting stream is a non-element stream.
    ///
    /// # Examples
    ///
    /// Transformations will be applied at any depth:
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <span id="outer" class="name">Foo</span>
    ///     <div>
    ///         <span id="inner" class="name">Bar</span>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select(".name", |html| html
    ///         .add_class("highlight")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<span id="outer" class="name highlight">"#));
    /// assert!(output.contains(r#"<span id="inner" class="name highlight">"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    ///
    /// Transformations are not recursive:
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div class="outer main">
    ///         <div class="inner main">Content</div>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select(".main", |html| html
    ///         .add_class("highlight")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div class="outer main highlight">"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<div class="inner main">"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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
    
    /// Apply transformations to the first matching element in a stream.
    ///
    /// This function will match elements at any depth, but the transformation is not
    /// recursive. The stream provided to the transformation closure will be an element stream.
    /// The resulting stream is a non-element stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    /// use html5_fuser::text::{ Data };
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let title = Data::from_unencoded_str("New Title");
    /// let output = format!("{}", template.transform(|html| html
    ///     // No need to .clone() the title since we're in a FnOnce
    ///     .select_once("title", |html| html.replace_contents(title))
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn select_once<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SelectAnyOnce<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnOnce(Api<'tb, modifier::select::BuildElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SelectAnyOnce::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    /// Apply transformations to each matching top-level element in a stream.
    ///
    /// This function will match elements at the top level of the stream.
    /// The stream provided to the transformation closure will be an element stream.
    /// The resulting stream is a non-element stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <span id="outer" class="name">Foo</span>
    ///     <div>
    ///         <span id="inner" class="name">Bar</span>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select_direct(".name", |html| html
    ///         .add_class("highlight")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<span id="outer" class="name highlight">"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<span id="inner" class="name">"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Apply transformations to the first matching top-level element in a stream.
    ///
    /// This function will match elements at the top level of the stream.
    /// The stream provided to the transformation closure will be an element stream.
    /// The resulting stream is a non-element stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <span id="outer1" class="name">Foo</span>
    ///     <div>
    ///         <span id="inner" class="name">Baz</span>
    ///     </div>
    ///     <span id="outer2" class="name">Foo</span>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select_direct_once(".name", |html| html
    ///         .add_class("highlight")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<span id="outer1" class="name highlight">"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<span id="outer2" class="name">"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<span id="inner" class="name">"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn select_direct_once<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SelectDirectOnce<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnOnce(Api<'tb, modifier::select::BuildElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SelectDirectOnce::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    /// Removes all elements of the substream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div><h1>Title</h1><p>Content</p><p>More Content</p></div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("p", |html| html.remove())
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div><h1>Title</h1></div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn remove(self) -> Api<'t, modifier::remove::Remove<S>> {
        Api::pack(modifier::remove::Remove::new(self.stream))
    }

    /// Emit another stream before the current one.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div><p>Content</p></div>
    /// "#, ParseOptions::default())?;
    ///
    /// let header_template = Template::from_str(
    ///     r#"<h1>Title</h1>"#,
    ///     ParseOptions::default(),
    /// )?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("p", |html| html
    ///         .prepend(&header_template)
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div><h1>Title</h1><p>Content</p></div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn prepend<N>(self, stream: N) -> Api<'t, modifier::prepend::Prepend<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::Prepend::new(self.stream, stream.into_stream()))
    }

    /// Emit another stream after the current one.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div><h1>Title</h1></div>
    /// "#, ParseOptions::default())?;
    ///
    /// let content_template = Template::from_str(
    ///     r#"<p>Content</p>"#,
    ///     ParseOptions::default(),
    /// )?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("h1", |html| html
    ///         .append(&content_template)
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div><h1>Title</h1><p>Content</p></div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn append<N>(self, stream: N) -> Api<'t, modifier::append::Append<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::Append::new(self.stream, stream.into_stream()))
    }

    /// Replace the current stream with a different one.
    ///
    /// This effectively consumes the existing stream and drops all events, then emits
    /// the events from the new stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div><div id="title-placeholder"/>Content</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let title_template = Template::from_str(
    ///     r#"<h1>Title</h1>"#,
    ///     ParseOptions::default(),
    /// )?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("#title-placeholder", |html| html
    ///         .replace(&title_template)
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div><h1>Title</h1>Content</div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn replace<N>(self, stream: N) -> Api<'t, modifier::replace::Replace<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::Replace::new(self.stream, stream.into_stream()))
    }

    /// Repeats a stream for each item of an iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <ul><li>Item</li></ul>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("ul", |html| html
    ///         .subselect("li", |html| html
    ///             .repeat(1..3, |html, value| html
    ///                 .append_contents(" ")
    ///                 .append_contents(value)
    ///             )
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<ul><li>Item 1</li><li>Item 2</li></ul>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn repeat<I, B, R>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::Repeat<S, I::IntoIter, B>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, modifier::repeat::MaybeElementTemplate<S>>, I::Item)
            -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::repeat::Repeat::new(self.stream, iter.into_iter(), builder))
    }
}

type SubSelectAny<S, M, B> =
    modifier::Fallible<modifier::select::SubSelectAny<S, M, B>>;

type SubSelectDirect<S, M, B> =
    modifier::Fallible<modifier::select::SubSelectDirect<S, M, B>>;

type SubSelectAnyOnce<S, M, B> =
    modifier::Fallible<modifier::select::SubSelectAnyOnce<S, M, B>>;

type SubSelectDirectOnce<S, M, B> =
    modifier::Fallible<modifier::select::SubSelectDirectOnce<S, M, B>>;

/// This is the API implementation specific to element streams.
///
/// This kind of API is usually available on the streams passed in to `select` transform
/// specifiers.
impl<'t, S> Api<'t, S> where S: event::ElementStream {

    /// Boxes the element stream.
    ///
    /// Similar to `into_boxed` but instead boxes the `ElementStream` allowing element
    /// transformations on the boxed stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{
    ///     Template, ParseOptions, StreamError, BoxedElementApi,
    /// };
    /// use html5_fuser::text::{ Data };
    ///
    /// fn transform_title<'a>(
    ///     html: BoxedElementApi<'a>,
    ///     title: &str,
    /// ) -> BoxedElementApi<'a> {
    ///     let title = Data::from_unencoded_str(title);
    ///     html.replace_contents(title)
    ///         .into_boxed_element()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("title", |html| html
    ///         .apply(|html|
    ///             transform_title(html.into_boxed_element(), "New Title")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn into_boxed_element(self) -> BoxedElementApi<'t> where S: 'static {
        Api::pack(Box::new(self.stream))
    }

    /// Box the stream and apply the transformation.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{
    ///     Template, ParseOptions, StreamError, BoxedElementApi,
    /// };
    /// use html5_fuser::text::{ Data };
    ///
    /// fn transform_title<'a>(
    ///     html: BoxedElementApi<'a>,
    ///     title: &str,
    /// ) -> BoxedElementApi<'a> {
    ///     let title = Data::from_unencoded_str(title);
    ///     html.replace_contents(title)
    ///         .into_boxed_element()
    /// }
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("title", |html| html
    ///         .apply_as_boxed_element(|html|
    ///             transform_title(html, "New Title")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn apply_as_boxed_element<B, R>(self, builder: B) -> Api<'t, R>
    where
        B: for<'tb> FnOnce(BoxedElementApi<'tb>) -> Api<'tb, R>,
        R: event::Stream,
        S: 'static,
    {
        builder(self.into_boxed_element())
    }

    /// Transform the contents of the current stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div id="one">Foo</div>
    ///     <div id="two">Foo</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("#one", |html| html
    ///         .subselect_contents(|html| html.remove())
    ///     )
    ///     // is the same as
    ///     .select("#two", |html| html
    ///         .remove_contents()
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<div id="one"></div>"#));
    /// assert!(output.contains(r#"<div id="two"></div>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn subselect_contents<B, R>(self, builder: B)
    -> Api<'t, modifier::select::SelectContent<S, B>>
    where
        B: for<'tb> FnOnce(Api<'tb, modifier::select::CurrentContent<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::select::SelectContent::new(self.stream, builder))
    }

    /// Apply transformation to all matching elements in the contents of the current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <body><div id="outer"><div id="inner">23</div></div></body>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .subselect("div", |html| html.remove())
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<body><div id="outer"></div></body>"#,
    /// ));
    /// 
    /// // With select the current element is reselected.
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .select("div", |html| html.remove())
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<body></body>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Apply transformation to the first matching element in the contents of the current
    /// element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div>
    ///         <div>First</div>
    ///         <div>Second</div>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .subselect_once("div", |html| html
    ///             .add_class("found")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<div class="found">First</div>"#));
    /// assert!(output.contains(r#"<div>Second</div>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn subselect_once<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SubSelectAnyOnce<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnOnce(Api<'tb, modifier::select::BuildSubElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SubSelectAnyOnce::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    /// Apply transformation to all matching elements in the contents of the current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div>
    ///         <div>First</div>
    ///         <div>Second</div>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .subselect_direct("div", |html| html
    ///             .add_class("found")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<div class="found">First</div>"#));
    /// assert!(output.contains(r#"<div class="found">Second</div>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Apply transformation to the first matching direct child element in the contents of the
    /// current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div>
    ///         <article><div>Lower</div></article>
    ///         <div>First</div>
    ///         <div>Second</div>
    ///     </div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .subselect_direct_once("div", |html| html
    ///             .add_class("found")
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<div>Lower</div>"#));
    /// assert!(output.contains(r#"<div class="found">First</div>"#));
    /// assert!(output.contains(r#"<div>Second</div>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn subselect_direct_once<M, B, R>(self, selector: M, builder: B)
    -> Api<'t, SubSelectDirectOnce<S, M::Selector, B>>
    where
        M: select::IntoSelector,
        B: for<'tb> FnOnce(Api<'tb, modifier::select::BuildSubElementStream<S>>) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::Fallible::new(match selector.into_selector() {
            Ok(selector) =>
                Ok(modifier::select::SubSelectDirectOnce::new(self.stream, selector, builder)),
            Err(error) => Err(error.into()),
        }))
    }

    /// Remove the contents of the current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <body>Some <em>content</em>.</body>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("body", |html| html.remove_contents())
    /// )?);
    ///
    /// assert!(output.contains(r#"<body></body>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn remove_contents(self) -> Api<'t, modifier::remove::RemoveContent<S>> {
        Api::pack(modifier::remove::RemoveContent::new(self.stream))
    }

    /// Prepend another stream to the contents of the current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("title", |html| html
    ///         .prepend_contents("MyApp: ")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>MyApp: Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn prepend_contents<N>(self, stream: N)
    -> Api<'t, modifier::prepend::PrependContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::prepend::PrependContent::new(self.stream, stream.into_stream()))
    }

    /// Append another stream to the contents of the current element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("title", |html| html
    ///         .append_contents(" - MyApp")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>Title - MyApp</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn append_contents<N>(self, stream: N)
    -> Api<'t, modifier::append::AppendContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::append::AppendContent::new(self.stream, stream.into_stream()))
    }

    /// Replace the contents of the current element with another stream.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <head><title>Title</title></head>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("title", |html| html
    ///         .replace_contents("New Title")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<title>New Title</title>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn replace_contents<N>(self, stream: N)
    -> Api<'t, modifier::replace::ReplaceContent<S, N::Stream>>
    where
        N: event::IntoStream,
    {
        Api::pack(modifier::replace::ReplaceContent::new(self.stream, stream.into_stream()))
    }

    /// Repeat the contents of the current element for each item in an iterator.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <ul><li>Item</li></ul>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("ul", |html| html
    ///         .repeat_contents(2..4, |html, value| html
    ///             .select_once("li", move |html| html
    ///                 .replace_contents(value)
    ///             )
    ///         )
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<li>2</li><li>3</li>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn repeat_contents<I, B, R>(self, iter: I, builder: B)
    -> Api<'t, modifier::repeat::RepeatContent<S, I::IntoIter, B>>
    where
        I: IntoIterator,
        B: for<'tb> FnMut(Api<'tb, template::TemplateStream>, I::Item) -> Api<'tb, R>,
        R: event::Stream,
    {
        Api::pack(modifier::repeat::RepeatContent::new(self.stream, iter.into_iter(), builder))
    }

    /// Add an attribute to the current element.
    ///
    /// Existing attributes of the same name will be kept.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <a id="home-link">Home</a>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select_once("a#home-link", |html| html
    ///         .add_attribute("href", "http://example.com/")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<a id="home-link" href="http://example.com/">Home</a>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Add an attribute without a value.
    ///
    /// Existing attributes of the same name will be kept.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <input id="info">
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select_once("input#info", |html| html
    ///         .add_empty_attribute("disabled")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<input id="info" disabled>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Add a value to an existing attribute.
    ///
    /// The value will only be added to the first occurence of the attribute. If none exists
    /// yet, a new attribute with the passed value will be created.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div style="color: red">Foo</div>
    ///     <div>Bar</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .add_to_attribute("style", "padding: 0", "; ")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div style="color: red; padding: 0">Foo</div>"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<div style="padding: 0">Bar</div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Replace existing attributes with a new one.
    ///
    /// If no existing attribute was found, no new attribute will be created.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <a href="foo.html">Foo</a>
    ///     <a>Bar</a>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("a", |html| html
    ///         .replace_attribute("href", "new.html")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<a href="new.html">Foo</a>"#));
    /// assert!(output.contains(r#"<a>Bar</a>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Set an attribute to a specific value.
    ///
    /// All existing attributes with the same name will be removed. If no attribute with
    /// the given name exists, one will still be added.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <a href="foo.html">Foo</a>
    ///     <a>Bar</a>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("a", |html| html
    ///         .set_attribute_value("href", "new.html")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<a href="new.html">Foo</a>"#));
    /// assert!(output.contains(r#"<a href="new.html">Bar</a>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn set_attribute_value<N, V>(self, name: N, value: V)
    -> Api<'t, modifier::Fallible<modifier::attribute::SetAttribute<S>>>
    where
        N: text::IntoIdentifier,
        V: text::IntoValue,
    {
        self.set_attribute(name, Some(value))
    }

    /// Set an attribute with an optional value.
    ///
    /// All existing attributes with the same name will be removed. If no attribute with
    /// the given name exists, one will still be added.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <a href="foo.html">Foo</a>
    ///     <a>Bar</a>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("a", |html| html
    ///         .set_attribute("href", Some("new.html"))
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<a href="new.html">Foo</a>"#));
    /// assert!(output.contains(r#"<a href="new.html">Bar</a>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn set_attribute<N, V>(self, name: N, value: Option<V>)
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
                    value.map(text::IntoValue::into_value),
                ))
                .map_err(Into::into)
        ))
    }

    /// Set an attribute with the empty value.
    ///
    /// All existing attributes with the same name will be removed. If no attribute with
    /// the given name exists, one will still be added.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <input name="one" disabled>
    ///     <input name="two">
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("input", |html| html
    ///         .set_empty_attribute("disabled")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<input name="one" disabled>"#));
    /// assert!(output.contains(r#"<input name="two" disabled>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Remove all attributes with a specific name.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <input name="info" disabled>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("input", |html| html
    ///         .remove_attribute("disabled")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<input name="info">"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Add a class to the element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <input id="one" class="required">
    ///     <input id="two">
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("input", |html| html
    ///         .add_class("field")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<input id="one" class="required field">"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<input id="two" class="field">"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Remove a class from the element.
    ///
    /// The `class` element will be removed completely if there are no more classes.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div id="one" class="warning">Foo</div>
    ///     <div id="two" class="warning msg">Bar</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("div", |html| html
    ///         .remove_class("warning")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(
    ///     r#"<div id="one">Foo</div>"#,
    /// ));
    /// assert!(output.contains(
    ///     r#"<div id="two" class="msg">Bar</div>"#,
    /// ));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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

    /// Remove the ID attribute from the element.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <div id="foo">Foo</div>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select("#foo", |html| html
    ///         // Remove the `id` attribute we just selected on.
    ///         .remove_id()
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<div>Foo</div>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
    pub fn remove_id(self)
    -> Api<'t, modifier::attribute::RemoveAttribute<S>> {
        Api::pack(modifier::attribute::RemoveAttribute::new(
            self.stream,
            text::Identifier::from_static_str("id").expect("remove_id attribute identifier"),
        ))
    }

    /// Set the ID attribute of the element.
    ///
    /// An existing ID attribute will be overridden.
    ///
    /// # Examples
    ///
    /// ```
    /// # use std::error;
    /// # fn run() -> Result<(), Box<error::Error>> {
    /// use html5_fuser::{ Template, ParseOptions };
    ///
    /// let template = Template::from_str(r#"
    ///     <p>Foo</p>
    ///     <p>Bar</p>
    /// "#, ParseOptions::default())?;
    ///
    /// let output = format!("{}", template.transform(|html| html
    ///     .select_direct_once("p", |html| html
    ///         .set_id("first-paragraph")
    ///     )
    /// )?);
    ///
    /// assert!(output.contains(r#"<p id="first-paragraph">Foo</p>"#));
    /// assert!(output.contains(r#"<p>Bar</p>"#));
    /// # Ok(()) }
    /// # fn main() { run().unwrap() }
    /// ```
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
