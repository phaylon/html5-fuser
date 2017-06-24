
//! Consume and drop all stream events.

use event;
use modifier;
use transform;
use builder;

/// Removes all items in a stream.
#[derive(Debug)]
pub struct Remove<S> {
    stream: S,
}

impl<S> Remove<S> where S: event::Stream {

    pub(crate) fn new(stream: S) -> Remove<S> {
        Remove { stream }
    }
}

impl<S> event::Stream for Remove<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        while let Some(_) = self.stream.next_event()? {}
        Ok(None)
    }
}

/// Removes all contents of the current element.
pub struct RemoveContent<S> where S: event::ElementStream {
    stream: modifier::select::SelectContent<S, Builder>,
}

impl<S> RemoveContent<S> where S: event::ElementStream {

    pub(crate) fn new(stream: S) -> RemoveContent<S> {
        RemoveContent {
            stream: modifier::select::SelectContent::new(stream, Builder),
        }
    }
}

impl<S> event::ElementStream for RemoveContent<S> where S: event::ElementStream {}

impl<S> event::Stream for RemoveContent<S> where S: event::ElementStream {

    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

struct Builder;

impl<S> builder::BuildOnce<modifier::select::CurrentContent<S>> for Builder
where
    S: event::ElementStream,
{
    type Stream = Remove<modifier::select::CurrentContent<S>>;

    fn build_once(self, stream: transform::Api<modifier::select::CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(Remove::new(stream.unpack()))
    }
}

#[cfg(test)]
mod tests {

    test_group!(remove_contents:
        "remove contents" => transform_test!(
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a><b></b>45<c>67</c><b></b></a>",
            |html| html.select("b", |html| html.remove_contents()),
        ),
        "ensure element stream" => transform_test!(
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a><b></b>45<c>67</c><b></b></a>",
            |html| html.select("b", |html| html.remove_contents().into_boxed_element()),
        ),
    );

    test_group!(remove:
        "remove" => transform_test!(
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a>45<c>67</c></a>",
            |html| html.select("b", |html| html.remove()),
        ),
    );
}
