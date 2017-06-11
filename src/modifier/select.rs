
use std::rc;
use std::cell;
use std::marker;

use event;
use select;
use transform;
use text;
use modifier;

type ContentSource<S> = rc::Rc<cell::RefCell<modifier::Peek<modifier::Expand<S>>>>;

#[derive(Debug)]
enum ActorState<S, X, B> {
    Scanning {
        stream: S,
        builder: B,
    },
    Active {
        stream: X,
    },
    Skipped {
        stream: S,
    },
}

type GenericActorState<S, B, BS> =
    ActorState<
        ContentSource<S>,
        modifier::Combine<
            BS,
            ContentSource<S>,
        >,
        B,
    >;

#[derive(Debug)]
pub(super) struct ContentActorOnce<S, B> where B: transform::BuildOnce<ContentStream<S>> {
    state: modifier::State<GenericActorState<S, B, B::Stream>>,
}

impl<S, B> ContentActorOnce<S, B>
where
    S: event::Stream,
    B: transform::BuildOnce<ContentStream<S>>,
{
    pub(super) fn new(stream: S, builder: B) -> ContentActorOnce<S, B> {
        let stream = rc::Rc::new(cell::RefCell::new(
            modifier::Peek::new(modifier::Expand::new(stream)),
        ));
        ContentActorOnce {
            state: modifier::State::new(ActorState::Scanning { builder, stream }),
        }
    }
}

impl<S, B> event::Stream for ContentActorOnce<S, B>
where
    S: event::Stream,
    B: transform::BuildOnce<ContentStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.state.step(|state| match state {
            ActorState::Scanning { builder, mut stream } =>
                match stream.next_event_skip_noop()? {
                    Some(event::Event::VoidTag { tag, attributes }) =>
                        Ok(Some((
                            event::void(tag, attributes),
                            Some(ActorState::Skipped { stream }),
                        ))),
                    Some(event::Event::OpeningTag { tag, attributes }) =>
                        Ok(Some((
                            event::open(tag.clone(), attributes),
                            Some(ActorState::Active {
                                stream: modifier::Combine::new(
                                    builder.build_once(ContentStream::new(stream.clone(), tag)),
                                    stream,
                                ),
                            }),
                        ))),
                    other => Err(event::StreamError::expected_open(other)),
                },
            ActorState::Skipped { stream } =>
                modifier::passthrough(stream, |stream| ActorState::Skipped { stream }),
            ActorState::Active { stream } =>
                modifier::passthrough(stream, |stream| ActorState::Active { stream }),
        })
    }
}

#[derive(Debug)]
pub struct SelectContent<S, F, X>
where
    S: event::Stream,
    F: for<'t> FnOnce(transform::Api<'t, ContentStream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    stream: ContentActorOnce<S, F>,
}

impl<S, F, X> SelectContent<S, F, X>
where
    S: event::Stream,
    F: for<'t> FnOnce(transform::Api<'t, ContentStream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(stream: S, builder: F) -> SelectContent<S, F, X> {
        SelectContent {
            stream: ContentActorOnce::new(stream, builder),
        }
    }
}

impl<S, F, X> event::ElementStream for SelectContent<S, F, X>
where
    S: event::Stream,
    F: for<'t> FnOnce(transform::Api<'t, ContentStream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{}

impl<S, F, X> event::Stream for SelectContent<S, F, X>
where
    S: event::Stream,
    F: for<'t> FnOnce(transform::Api<'t, ContentStream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<S> ContentStream<S> where S: event::Stream {

    fn new(stream: ContentSource<S>, tag_name: text::Identifier) -> ContentStream<S> {
        ContentStream {
            state: modifier::State::new(ContentStreamState {
                stream,
                tag_name,
                level: 0,
            }),
        }
    }
}

#[derive(Debug)]
pub struct ContentStream<S> {
    state: modifier::State<ContentStreamState<S>>,
}

fn take_peeked<S>(stream: &ContentSource<S>) -> Option<event::Event>
where S: event::Stream {
    stream.borrow_mut().take_peeked()
}

impl<S> event::Stream for ContentStream<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {

        enum EventKind { Open, Close, Other }

        self.state.step(|ContentStreamState { stream, tag_name, level }| {
            let kind = {
                let mut stream = stream.borrow_mut();
                stream.peek(|event|
                    if event.is_opening_tag_for(&tag_name) {
                        EventKind::Open
                    } else if event.is_closing_tag_for(&tag_name) {
                        EventKind::Close
                    } else {
                        EventKind::Other
                    }
                )?
            };
            match kind {
                None => Err(event::StreamError::missing_close(tag_name.to_string())),
                Some(EventKind::Other) =>
                    Ok(Some((
                        take_peeked(&stream).expect("peeked other event"),
                        Some(ContentStreamState { stream, tag_name, level })
                    ))),
                Some(EventKind::Open) =>
                    Ok(Some((
                        take_peeked(&stream).expect("peeked open event"),
                        Some(ContentStreamState {
                            stream,
                            tag_name,
                            level: level + 1,
                        }),
                    ))),
                Some(EventKind::Close) =>
                    if level == 0 {
                        Ok(None)
                    } else {
                        Ok(Some((
                            take_peeked(&stream).expect("peeked close event"),
                            Some(ContentStreamState {
                                stream,
                                tag_name,
                                level: level - 1,
                            }),
                        )))
                    },
            }
        })
    }
}

#[derive(Debug)]
struct ContentStreamState<S> {
    stream: ContentSource<S>,
    tag_name: text::Identifier,
    level: usize,
}

#[derive(Debug)]
struct BuildSelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    selector: M,
    builder: F,
    _phantom: marker::PhantomData<S>,
}

impl<M, S, F, X> transform::BuildOnce<ContentStream<S>> for BuildSelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    type Stream = SelectDirect<M, ContentStream<S>, F, X>;

    fn build_once(self, stream: ContentStream<S>) -> Self::Stream {
        SelectDirect::new(self.selector, stream, self.builder)
    }
}

#[derive(Debug)]
struct BuildSelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    selector: M,
    builder: F,
    _phantom: marker::PhantomData<S>,
}

impl<M, S, F, X> transform::BuildOnce<ContentStream<S>> for BuildSelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    type Stream = SelectAny<M, ContentStream<S>, F, X>;

    fn build_once(self, stream: ContentStream<S>) -> Self::Stream {
        SelectAny::new(self.selector, stream, self.builder)
    }
}

#[derive(Debug)]
pub struct SubSelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    stream: ContentActorOnce<S, BuildSelectDirect<M, S, F, X>>,
}

impl<M, S, F, X> SubSelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(selector: M, stream: S, builder: F) -> SubSelectDirect<M, S, F, X> {
        SubSelectDirect {
            stream: ContentActorOnce::new(stream, BuildSelectDirect {
                selector,
                builder,
                _phantom: marker::PhantomData,
            }),
        }
    }
}

impl<M, S, F, X> event::Stream for SubSelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

#[derive(Debug)]
pub struct SubSelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    stream: ContentActorOnce<S, BuildSelectAny<M, S, F, X>>,
}

impl<M, S, F, X> SubSelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(selector: M, stream: S, builder: F) -> SubSelectAny<M, S, F, X> {
        SubSelectAny {
            stream: ContentActorOnce::new(stream, BuildSelectAny {
                selector,
                builder,
                _phantom: marker::PhantomData,
            }),
        }
    }
}

impl<M, S, F, X> event::Stream for SubSelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<ContentStream<S>>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

#[derive(Debug)]
enum SelectState<S> {
    Scanning,
    Active {
        stream: S,
    },
}

trait Restrict {

    fn feed(&mut self, _event: &event::Event) -> bool { true }

    fn reset(&mut self) {}
}

#[derive(Debug)]
struct Any;

impl Restrict for Any {}

#[derive(Debug)]
struct Direct {
    level: usize,
}

impl Restrict for Direct {

    fn reset(&mut self) {
        self.level = 0;
    }

    fn feed(&mut self, event: &event::Event) -> bool {
        let status = self.level == 0;
        if event.is_opening_tag() {
            self.level += 1;
        } else if event.is_closing_tag() {
            self.level -= 1;
        }
        status
    }
}

#[derive(Debug)]
pub struct SelectDirect<M, S, F, X> {
    stream: Select<M, S, F, X, Direct>,
}

impl<M, S, F, X> event::Stream for SelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<M, S, F, X> SelectDirect<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(selector: M, stream: S, builder: F) -> SelectDirect<M, S, F, X> {
        SelectDirect {
            stream: Select {
                restrict: Direct { level: 0 },
                selector,
                stream: rc::Rc::new(cell::RefCell::new(stream)),
                builder,
                state: modifier::State::new(SelectState::Scanning),
            },
        }
    }
}

#[derive(Debug)]
pub struct SelectAny<M, S, F, X> {
    stream: Select<M, S, F, X, Any>,
}

impl<M, S, F, X> event::Stream for SelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<M, S, F, X> SelectAny<M, S, F, X>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
{
    pub(crate) fn new(selector: M, stream: S, builder: F) -> SelectAny<M, S, F, X> {
        SelectAny {
            stream: Select {
                restrict: Any,
                selector,
                stream: rc::Rc::new(cell::RefCell::new(stream)),
                builder,
                state: modifier::State::new(SelectState::Scanning),
            },
        }
    }
}

#[derive(Debug)]
struct Select<M, S, F, X, R> {
    restrict: R,
    selector: M,
    stream: rc::Rc<cell::RefCell<S>>,
    builder: F,
    state: modifier::State<SelectState<X>>,
}

fn matches_event<M>(selector: &M, event: &event::Event) -> bool
where M: select::Selector {
    match *event {
        event::Event::OpeningTag { ref tag, ref attributes, .. }
        | event::Event::SelfClosedTag { ref tag, ref attributes, .. }
        | event::Event::VoidTag { ref tag, ref attributes, .. } =>
            selector.matches(tag, attributes),
        _ => false,
    }
}

impl<M, S, F, X, R> event::Stream for Select<M, S, F, X, R>
where
    M: select::Selector,
    S: event::Stream,
    F: for<'t> FnMut(transform::Api<'t, Stream<S>>) -> transform::Api<'t, X>,
    X: event::Stream,
    R: Restrict,
{
    fn next_event(&mut self) -> event::StreamResult {
        let Select {
            ref selector,
            ref stream,
            ref mut builder,
            ref mut state,
            ref mut restrict,
        } = *self;
        state.step(|state| match state {
            SelectState::Scanning => match stream.borrow_mut().next_event_skip_noop()? {
                None => Ok(None),
                Some(event) => {
                    let active = restrict.feed(&event);
                    if active
                        && event.is_self_contained_tag()
                        && matches_event(selector, &event)
                    {
                        Ok(Some((
                            event::noop(),
                            Some(SelectState::Active {
                                stream: transform::apply(stream_event(event), builder),
                            }),
                        )))
                    } else if active
                        && event.is_opening_tag()
                        && matches_event(selector, &event)
                    {
                        Ok(Some((
                            event::noop(),
                            Some(SelectState::Active {
                                stream: transform::apply(stream_range(event, stream), builder),
                            }),
                        )))
                    } else {
                        Ok(Some((event, Some(SelectState::Scanning))))
                    }
                },
            },
            SelectState::Active { mut stream } => match stream.next_event_skip_noop()? {
                None => {
                    restrict.reset();
                    Ok(Some((event::noop(), Some(SelectState::Scanning))))
                },
                Some(event) => Ok(Some((event, Some(SelectState::Active { stream })))),
            },
        })
    }
}

#[derive(Debug)]
pub struct Stream<S> {
    variant: modifier::State<StreamVariant<rc::Rc<cell::RefCell<S>>>>,
}

impl<S> event::ElementStream for Stream<S> where Stream<S>: event::Stream {}

fn stream_range<S>(event: event::Event, stream: &rc::Rc<cell::RefCell<S>>) -> Stream<S> {
    Stream {
        variant: modifier::State::new(StreamVariant::Start {
            event,
            stream: stream.clone(),
        }),
    }
}

fn stream_event<S>(event: event::Event) -> Stream<S> {
    Stream {
        variant: modifier::State::new(StreamVariant::SingleEvent { event }),
    }
}

#[derive(Debug)]
enum StreamVariant<S> {
    SingleEvent {
        event: event::Event,
    },
    Start {
        event: event::Event,
        stream: S,
    },
    UntilClose {
        tag_name: text::Identifier,
        stream: S,
        depth: usize,
    },
}

impl<S> event::Stream for Stream<S>
where
    S: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.variant.step(|state| match state {
            StreamVariant::SingleEvent { event } =>
                Ok(Some((event, None))),
            StreamVariant::Start { event, stream } => {
                let identifier = event.opening_tag_name()
                    .expect("opening tag name")
                    .clone();
                Ok(Some((
                    event,
                    Some(StreamVariant::UntilClose {
                        tag_name: identifier,
                        stream,
                        depth: 0,
                    }),
                )))
            },
            StreamVariant::UntilClose { tag_name, stream, depth } => {
                let next = stream.borrow_mut().next_event_skip_noop()?;
                match next {
                    None => Err(event::StreamError::missing_close(tag_name.to_string())),
                    Some(event) =>
                        if event.is_opening_tag_for(&tag_name) {
                            Ok(Some((
                                event,
                                Some(StreamVariant::UntilClose {
                                    tag_name,
                                    stream,
                                    depth: depth + 1,
                                })
                            )))
                        } else if event.is_closing_tag_for(&tag_name) {
                            if depth == 0 {
                                Ok(Some((event, None)))
                            } else {
                                Ok(Some((
                                    event,
                                    Some(StreamVariant::UntilClose {
                                        tag_name,
                                        stream,
                                        depth: depth - 1,
                                    })
                                )))
                            }
                        } else {
                            Ok(Some((
                                event,
                                Some(StreamVariant::UntilClose { tag_name, stream, depth }),
                            )))
                        },
                }
            },
        })
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn subselect_direct() {
        test_transform!(
            Default::default(),
            "<a><a>23</a></a><a>23</a>",
            "<a><a>99</a></a><a>23</a>",
            |html| html
                .select_direct(::select::Tag::from_str("a").unwrap(), |html| html
                    .subselect_direct(::select::Tag::from_str("a").unwrap(), |html| html
                        .replace_contents("99")
                    )
                )
        );
        test_transform!(
            Default::default(),
            "<c></c><d /><a><a>23</a></a><a>23</a>",
            "<c></c><d /><a>99</a><a>99</a>",
            |html| html
                .select_direct(::select::Tag::from_str("a").unwrap(), |html| html
                    .select_direct(::select::Tag::from_str("a").unwrap(), |html| html
                        .replace_contents("99")
                    )
                )
        );
    }

    #[test]
    fn select_direct() {
        test_transform!(
            Default::default(),
            "<a><b>23</b></a><b>47</b>",
            "<a><b>23</b></a><b>99</b>",
            |html| html
                .select_direct(::select::Tag::from_str("b").unwrap(), |html| html
                    .replace_contents("99")
                )
        );
    }

    #[test]
    fn subselect() {
        test_transform!(
            Default::default(),
            "<a><a><a>23</a></a></a>",
            "<a><a><a>99</a></a></a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .subselect(::select::Tag::from_str("a").unwrap(), |html| html
                        .subselect(::select::Tag::from_str("a").unwrap(), |html| html
                            .replace_contents("99")                            
                        )
                    )
                )
        );
        test_transform!(
            Default::default(),
            "<a><a>23</a></a>",
            "<a><a>23</a></a>",
            |html| html
                .select(::select::Tag::from_str("a").unwrap(), |html| html
                    .subselect(::select::Tag::from_str("a").unwrap(), |html| html
                        .subselect(::select::Tag::from_str("a").unwrap(), |html| html
                            .replace_contents("99")                            
                        )
                    )
                )
        );
    }

    #[test]
    fn select_contents() {
        test_transform!(
            Default::default(),
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a><b></b>45<c>67</c><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .subselect_contents(|html| html.remove())
                )
        );
    }

    #[test]
    fn select_roundtrip() {
        let original = "<a><b><c><b>foo</b></c></b></a>";
        test_transform!(
            Default::default(),
            original,
            original,
            |html| html.select(::select::Tag::from_str("b").unwrap(), |elem| elem)
        );
    }
}
