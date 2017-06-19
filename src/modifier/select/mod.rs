
use std::rc;
use std::cell;

use event;
use select;
use transform;
use modifier;
use builder;

pub(crate) mod element;
pub(crate) mod select_any;
pub(crate) mod select_direct;
pub(crate) mod select_contents;
pub(crate) mod subselect_any;
pub(crate) mod subselect_direct;

pub use self::element::{ SingleElement };
pub use self::select_any::{ SelectAny };
pub use self::select_direct::{ SelectDirect };
pub use self::select_contents::{ SelectContent, CurrentContent };
pub use self::subselect_any::{ SubSelectAny };
pub use self::subselect_direct::{ SubSelectDirect };

type Shared<S> = rc::Rc<cell::RefCell<S>>;

trait Restrict {

    fn include(&self, level: &modifier::Level) -> bool;
}

#[derive(Debug)]
struct DirectOnly;

impl Restrict for DirectOnly {

    fn include(&self, level: &modifier::Level) -> bool { level.is_top_level() }
}

#[derive(Debug)]
struct NoRestriction;

impl Restrict for NoRestriction {

    fn include(&self, _level: &modifier::Level) -> bool { true }
}

pub(crate) type BuildElementStream<S> = element::SingleElement<Shared<S>>;

pub(crate) type BuildSubElementStream<S> = BuildElementStream<CurrentContent<S>>;

#[derive(Debug)]
struct Select<S, M, B, R> where B: builder::BuildMut<BuildElementStream<S>> {
    source: Shared<S>,
    matcher: M,
    builder: B,
    restriction: R,
    state: modifier::State<SelectState<B::Stream>>,
}

impl<S, M, B, R> Select<S, M, B, R>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
    R: Restrict,
{
    fn new(source: S, matcher: M, builder: B, restriction: R) -> Select<S, M, B, R> {
        Select {
            matcher,
            builder,
            restriction,
            source: rc::Rc::new(cell::RefCell::new(source)),
            state: modifier::State::new(SelectState::Scanning { level: modifier::Level::new() }),
        }
    }
}

impl<S, M, B, R> event::ElementStream for Select<S, M, B, R>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
    R: Restrict,
    B::Stream: event::ElementStream,
{}

impl<S, M, B, R> event::Stream for Select<S, M, B, R>
where
    S: event::Stream,
    M: select::Selector,
    B: builder::BuildMut<BuildElementStream<S>>,
    R: Restrict,
{
    fn next_event(&mut self) -> event::StreamResult {
        let Select {
            ref mut state,
            ref mut builder,
            ref mut source,
            ref restriction,
            ref matcher,
        } = *self;
        state.step(|state| match state {
            SelectState::Scanning { level } => {
                let event = match source.next_event_skip_noop()? {
                    Some(event) => event,
                    None => return Ok(None),
                };
                if restriction.include(&level) && matches_event(matcher, &event) {
                    let element = SingleElement::new(source.clone(), event);
                    let new_stream = transform::build_mut(element, builder);
                    let state = SelectState::Active { stream: new_stream, level };
                    Ok(Some((event::noop(), Some(state))))
                } else {
                    let new_level = level.adjust(&event)?;
                    let state = SelectState::Scanning { level: new_level };
                    Ok(Some((event, Some(state))))
                }
            },
            SelectState::Active { mut stream, level } => match stream.next_event()? {
                None => Ok(Some((event::noop(), Some(SelectState::Scanning { level })))),
                Some(event) => Ok(Some((event, Some(SelectState::Active { stream, level })))),
            },
        })
    }
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

#[derive(Debug)]
enum SelectState<O> {
    Scanning {
        level: modifier::Level,
    },
    Active {
        level: modifier::Level,
        stream: O,
    },
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
