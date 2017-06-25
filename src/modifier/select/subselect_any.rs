
use event;
use select;
use transform;
use builder;

use super::{
    SelectAny, SelectAnyOnce, BuildSubElementStream, SelectContent, CurrentContent,
};

/// Select a substream at any level in the current elements children.
pub struct SubSelectAny<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    stream: SelectContent<S, Builder<M, B>>,
}

impl<S, M, B> SubSelectAny<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    pub(crate) fn new(stream: S, selector: M, select_builder: B) -> SubSelectAny<S, M, B> {
        SubSelectAny {
            stream: SelectContent::new(stream, Builder { selector, select_builder }),
        }
    }
}

impl<S, M, B> event::ElementStream for SubSelectAny<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{}

impl<S, M, B> event::Stream for SubSelectAny<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildMut<BuildSubElementStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

/// Select a substream at any level in the current elements children but only once.
pub struct SubSelectAnyOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{
    stream: SelectContent<S, BuilderOnce<M, B>>,
}

impl<S, M, B> SubSelectAnyOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{
    pub(crate) fn new(stream: S, selector: M, select_builder: B) -> SubSelectAnyOnce<S, M, B> {
        SubSelectAnyOnce {
            stream: SelectContent::new(stream, BuilderOnce { selector, select_builder }),
        }
    }
}

impl<S, M, B> event::ElementStream for SubSelectAnyOnce<S, M, B>
where
    S: event::ElementStream,
    M: select::Selector,
    B: builder::BuildOnce<BuildSubElementStream<S>>,
{}

impl<S, M, B> event::Stream for SubSelectAnyOnce<S, M, B>
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
    type Stream = SelectAny<CurrentContent<S>, M, B>;

    fn build_once(self, stream: transform::Api<CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(SelectAny::new(stream.unpack(), self.selector, self.select_builder))
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
    type Stream = SelectAnyOnce<CurrentContent<S>, M, B>;

    fn build_once(self, stream: transform::Api<CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(SelectAnyOnce::new(
            stream.unpack(),
            self.selector,
            self.select_builder,
        ))
    }
}

#[cfg(test)]
mod tests {

    test_group!(subselect:
        "2 levels append" => transform_test!(
            "<b><b>23</b></b><b>45</b>",
            "<b><b>2399</b></b><b>45</b>",
            |html| html.select("b", |html| html
                .subselect("b", |html| html.append_contents(99)),
            ),
        ),
        "ensure element stream" => transform_test!(
            "<b><b>23</b></b><b>45</b>",
            "<b><b>2399</b></b><b>45</b>",
            |html| html.select("b", |html| html
                .subselect("b", |html| html.append_contents(99).into_boxed_element()),
            ),
        ),
        "three levels match" => transform_test!(
            "<a><a><a>23</a></a></a>",
            "<a><a><a>99</a></a></a>",
            |html| html.select("a", |html| html.subselect("a", |html| html
                .subselect("a", |html| html.replace_contents("99"))
            )),
        ),
        "three levels non-match" => transform_test!(
            "<a><a>23</a></a>",
            "<a><a>23</a></a>",
            |html| html.select("a", |html| html.subselect("a", |html| html
                .subselect("a", |html| html.replace_contents("99"))
            )),
        ),
    );

    test_group!(subselect_once:
        "below select_once" => transform_test!(
            "<b><b>23</b><b>45</b></b><b>67<b>89</b></b>",
            "<b><b>2399</b><b>45</b></b><b>67<b>89</b></b>",
            |html| html.select_once("b", |html| html
                .subselect_once("b", |html| html.append_contents(99)),
            ),
        ),
        "below select" => transform_test!(
            "<b><b>23</b><b>45</b></b><b>67<b>89</b></b>",
            "<b><b>2399</b><b>45</b></b><b>67<b>8999</b></b>",
            |html| html.select("b", |html| html
                .subselect_once("b", |html| html.append_contents(99)),
            ),
        ),
        "ensure element stream" => transform_test!(
            "<b><b>23</b><b>45</b></b><b>67<b>89</b></b>",
            "<b><b>2399</b><b>45</b></b><b>67<b>8999</b></b>",
            |html| html.select("b", |html| html
                .subselect_once("b", |html| html.append_contents(99).into_boxed_element()),
            ),
        ),
    );
}
