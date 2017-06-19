
use event;
use select;
use transform;
use builder;

use super::{
    SelectDirect, BuildSubElementStream, SelectContent, CurrentContent,
};

pub struct SubSelectDirect<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    stream: SelectContent<S, Builder<M, B>>,
}

impl<S, M, B> SubSelectDirect<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    pub(crate) fn new(stream: S, selector: M, select_builder: B) -> SubSelectDirect<S, M, B> {
        SubSelectDirect {
            stream: SelectContent::new(stream, Builder { selector, select_builder }),
        }
    }
}

impl<S, M, B> event::ElementStream for SubSelectDirect<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{}

impl<S, M, B> event::Stream for SubSelectDirect<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

struct Builder<M, B> {
    select_builder: B,
    selector: M,
}

impl<S, M, B> builder::BuildOnce<CurrentContent<S>> for Builder<M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    type Stream = SelectDirect<CurrentContent<S>, M, B>;

    fn build_once(self, stream: transform::Api<CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(SelectDirect::new(
            stream.unpack(),
            self.selector,
            self.select_builder,
        ))
    }
}


