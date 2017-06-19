
use event;
use select;
use builder;

use super::{ Select, BuildElementStream, DirectOnly };

pub struct SelectDirect<S, M, B>
where
    B: builder::BuildMut<BuildElementStream<S>>,
{
    select: Select<S, M, B, DirectOnly>,
}

impl<S, M, B> SelectDirect<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
{
    pub(crate) fn new(stream: S, matcher: M, builder: B) -> SelectDirect<S, M, B> {
        SelectDirect {
            select: Select::new(stream, matcher, builder, DirectOnly),
        }
    }
}

impl<S, M, B> event::Stream for SelectDirect<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.select.next_event()
    }
}

impl<S, M, B> event::ElementStream for SelectDirect<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildMut<BuildElementStream<S>>,
    B::Stream: event::ElementStream,
{}


