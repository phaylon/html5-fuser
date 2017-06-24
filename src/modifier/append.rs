
//! Append another stream.

use event;
use modifier;
use transform;
use builder;

/// Appends another stream after the current.
#[derive(Debug)]
pub struct Append<S, N> {
    stream: modifier::Combine<S, N>,
}

impl<S, N> Append<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> Append<S, N> {
        Append {
            stream: modifier::Combine::new(stream, new_stream),
        }
    }
}

impl<S, N> event::Stream for Append<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

/// Appends another stream to the contents of the current element.
pub struct AppendContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    stream: modifier::select::SelectContent<S, Builder<N>>,
}

impl<S, N> AppendContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> AppendContent<S, N> {
        AppendContent {
            stream: modifier::select::SelectContent::new(stream, Builder { new_stream }),
        }
    }
}

impl<S, N> event::ElementStream for AppendContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{}

impl<S, N> event::Stream for AppendContent<S, N>
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
    type Stream = Append<modifier::select::CurrentContent<S>, N>;

    fn build_once(self, stream: transform::Api<modifier::select::CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(Append::new(stream.unpack(), self.new_stream))
    }
}

#[cfg(test)]
mod tests {

    test_group!(append_contents:
        "append to existing" => transform_test!(
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>42foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.append_contents("foo")),
        ),
        "append to empty" => transform_test!(
            "<a><b>23</b><c></c><b>93</b></a>",
            "<a><b>23</b><c>foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.append_contents("foo")),
        ),
        "ensure element stream" => transform_test!(
            "<a><b>23</b><c></c><b>93</b></a>",
            "<a><b>23</b><c>foo</c><b>93</b></a>",
            |html| html.select("c", |html| html.append_contents("foo").into_boxed_element()),
        ),
    );

    test_group!(append:
        "append" => transform_test!(
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>42</c>foo<b>93</b></a>",
            |html| html.select("c", |html| html.append("foo")),
        ),
    );
}

