
use event;
use select;
use builder;

use super::{ Select, SelectOnce, BuildElementStream, DirectOnly };

/// Select a substream at the toplevel only.
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

/// Select a substream at the toplevel only and only once.
pub struct SelectDirectOnce<S, M, B>
where
    B: builder::BuildOnce<BuildElementStream<S>>,
{
    select: SelectOnce<S, M, B, DirectOnly>,
}

impl<S, M, B> SelectDirectOnce<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildOnce<BuildElementStream<S>>,
{
    pub(crate) fn new(stream: S, matcher: M, builder: B) -> SelectDirectOnce<S, M, B> {
        SelectDirectOnce {
            select: SelectOnce::new(stream, matcher, builder, DirectOnly),
        }
    }
}

impl<S, M, B> event::Stream for SelectDirectOnce<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildOnce<BuildElementStream<S>>,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.select.next_event()
    }
}

#[cfg(test)]
mod tests {

    test_group!(select_direct:
        "match all direct" => transform_test!(
            "<p><b>23</b></p><b>45</b><b>67</b>",
            "<p><b>23</b></p><b>4599</b><b>6799</b>",
            |html| html.select_direct("b", |html| html.append_contents(99)),
        ),
        "ensure_element_stream" => transform_test!(
            "<b />",
            "<b />",
            |html| html.select_direct("b", |html| html.into_boxed_element()),
        ),
    );

    test_group!(select_direct_once:
        "match first" => transform_test!(
            "<p><b>23</b></p><b>45</b><b>67</b>",
            "<p><b>23</b></p><b>4599</b><b>67</b>",
            |html| html.select_direct_once("b", |html| html.append_contents(99)),
        ),
        "ensure_element_stream" => transform_test!(
            "<b />",
            "<b />",
            |html| html.select_direct_once("b", |html| html.into_boxed_element()),
        ),
    );
}
