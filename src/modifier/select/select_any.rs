
use event;
use select;
use builder;

use super::{ Select, BuildElementStream, NoRestriction };

/// Select a substream at any level.
pub struct SelectAny<S, M, B>
where
    B: builder::BuildMut<BuildElementStream<S>>,
{
    select: Select<S, M, B, NoRestriction>,
}

impl<S, M, B> SelectAny<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
{
    pub(crate) fn new(stream: S, matcher: M, builder: B) -> SelectAny<S, M, B> {
        SelectAny {
            select: Select::new(stream, matcher, builder, NoRestriction),
        }
    }
}

impl<S, M, B> event::Stream for SelectAny<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.select.next_event()
    }
}
