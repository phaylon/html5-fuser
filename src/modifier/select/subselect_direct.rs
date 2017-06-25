
use event;
use select;
use transform;
use builder;

use super::{
    SelectDirect, SelectDirectOnce, BuildSubElementStream, SelectContent, CurrentContent,
};

/// Select a substream on the toplevel of the elements children only.
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

/// Select a substream on the toplevel of the elements children only and only once.
pub struct SubSelectDirectOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{
    stream: SelectContent<S, BuilderOnce<M, B>>,
}

impl<S, M, B> SubSelectDirectOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{
    pub(crate) fn new(stream: S, selector: M, select_builder: B)
    -> SubSelectDirectOnce<S, M, B> {
        SubSelectDirectOnce {
            stream: SelectContent::new(stream, BuilderOnce { selector, select_builder }),
        }
    }
}

impl<S, M, B> event::ElementStream for SubSelectDirectOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{}

impl<S, M, B> event::Stream for SubSelectDirectOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
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

struct BuilderOnce<M, B> {
    select_builder: B,
    selector: M,
}

impl<S, M, B> builder::BuildOnce<CurrentContent<S>> for BuilderOnce<M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{
    type Stream = SelectDirectOnce<CurrentContent<S>, M, B>;

    fn build_once(self, stream: transform::Api<CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(SelectDirectOnce::new(
            stream.unpack(),
            self.selector,
            self.select_builder,
        ))
    }
}

#[cfg(test)]
mod tests {

    test_group!(subselect_direct:
        "multiple" => transform_test!(
            "<b><a><b>00</b></a><b><b>23</b></b><b>45</b></b>",
            "<b><a><b>00</b></a><b><b>23</b>99</b><b>4599</b></b>",
            |html| html.select("b", |html| html
                .subselect_direct("b", |html| html.append_contents(99)),
            ),
        ),
        "two levels direct match" => transform_test!(
            "<a><a>23</a></a><a>23</a>",
            "<a><a>99</a></a><a>23</a>",
            |html| html.select_direct("a", |html| html.subselect_direct("a", |html| html
                .replace_contents("99")
            )),
        ),
        "late match" => transform_test!(
            "<c></c><d /><a><a>23</a></a><a>23</a>",
            "<c></c><d /><a>99</a><a>99</a>",
            |html| html.select_direct("a", |html| html.select_direct("a", |html| html
                .replace_contents("99")
            )),
        ),
        "ensure element stream" => transform_test!(
            "<link>",
            "<link>",
            |html| html.select_direct("a", |html| html.select_direct("a", |html| html
                .into_boxed_element()
            )),
        ),
    );

    test_group!(subselect_direct_once:
        "multiple" => transform_test!(
            "<b><a><b>00</b></a><b><b>23</b></b><b>45</b></b>",
            "<b><a><b>00</b></a><b><b>23</b>99</b><b>45</b></b>",
            |html| html.select_once("b", |html| html
                .subselect_direct_once("b", |html| html.append_contents(99)),
            ),
        ),
        "ensure element stream" => transform_test!(
            "<link>",
            "<link>",
            |html| html.select_once("b", |html| html
                .subselect_direct_once("b", |html| html.into_boxed_element()),
            ),
        ),
    );
}
