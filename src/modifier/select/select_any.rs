
use event;
use select;
use builder;

use super::{ Select, SelectOnce, BuildElementStream, NoRestriction };

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

/// Select a substream at any level but only once.
pub struct SelectAnyOnce<S, M, B>
where
    B: builder::BuildOnce<BuildElementStream<S>>,
{
    select: SelectOnce<S, M, B, NoRestriction>,
}

impl<S, M, B> SelectAnyOnce<S, M, B>
where
    M: select::Selector,
    S: event::Stream,
    B: builder::BuildOnce<BuildElementStream<S>>,
{
    pub(crate) fn new(stream: S, matcher: M, builder: B) -> SelectAnyOnce<S, M, B> {
        SelectAnyOnce {
            select: SelectOnce::new(stream, matcher, builder, NoRestriction),
        }
    }
}

impl<S, M, B> event::Stream for SelectAnyOnce<S, M, B>
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

    test_group!(select:
        "select all" => transform_test!(
            "<p><b>23</b></p><b>45</b>",
            "<p><b>2399</b></p><b>4599</b>",
            |html| html.select("b", |html| html.append_contents(99)),
        ),
        "ensure element stream" => transform_test!(
            "<b />",
            "<b />",
            |html| html.select("b", |html| html.into_boxed_element()),
        ),
    );

    test_group!(select_once:
        "select first" => transform_test!(
            "<p><b>23</b></p><b>45</b>",
            "<p><b>2399</b></p><b>45</b>",
            |html| html.select_once("b", |html| html.append_contents(99)),
        ),
        "ensure element stream" => transform_test!(
            "<b />",
            "<b />",
            |html| html.select_once("b", |html| html.into_boxed_element()),
        ),
    );
}
