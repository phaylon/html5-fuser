
//! Replace a stream with another one.

use event;
use modifier;
use transform;
use builder;

/// Emit a different stream instead of the current one.
pub struct Replace<S, N> {
    stream: modifier::prepend::Prepend<modifier::remove::Remove<S>, N>,
}

impl<S, N> Replace<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> Replace<S, N> {
        Replace {
            stream: modifier::prepend::Prepend::new(
                modifier::remove::Remove::new(stream),
                new_stream,
            ),
        }
    }
}

impl<S, N> event::Stream for Replace<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

/// Emit a different stream for the contents of the current element.
pub struct ReplaceContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    stream: modifier::select::SelectContent<S, Builder<N>>,
}

impl<S, N> ReplaceContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> ReplaceContent<S, N> {
        ReplaceContent {
            stream: modifier::select::SelectContent::new(stream, Builder { new_stream }),
        }
    }
}

impl<S, N> event::ElementStream for ReplaceContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{}

impl<S, N> event::Stream for ReplaceContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

struct Builder<N> {
    new_stream: N,
}

impl<S, N> builder::BuildOnce<modifier::select::CurrentContent<S>> for Builder<N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    type Stream = Replace<modifier::select::CurrentContent<S>, N>;

    fn build_once(self, stream: transform::Api<modifier::select::CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(Replace::new(stream.unpack(), self.new_stream))
    }
}

#[cfg(test)]
mod tests {

    test_group!(replace_contents:
        "void element" => transform_test!(
            "<a><b>23</b><link><b>93</b></a>",
            "<a><b>23</b><link><b>93</b></a>",
            |html| html.select("link", |html| html.replace_contents("foo")),
        ),
        "self closed element" => transform_test!(
            "<a><b>23</b><c/><b>93</b></a>",
            "<a><b>23</b><c>foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.replace_contents("foo")),
        ),
        "normal element" => transform_test!(
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.replace_contents("foo")),
        ),
        "ensure element stream" => transform_test!(
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.replace_contents("foo").into_boxed_element()),
        ),
    );

    test_group!(replace:
        "replace" => transform_test!(
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b>foo<b>93</b></a>",
            |html| html.select("c", |html| html.replace("foo")),
        ),
    );
}
