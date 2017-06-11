
use event;
use modifier;
use transform;

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

#[derive(Debug)]
pub struct RemoveContents<S> where S: event::Stream {
    stream: modifier::select::ContentActorOnce<S, BuildRemove>,
}

impl<S> RemoveContents<S> where S: event::Stream {

    pub(crate) fn new(stream: S) -> RemoveContents<S> {
        RemoveContents {
            stream: modifier::select::ContentActorOnce::new(stream, BuildRemove),
        }
    }
}

impl<S> event::Stream for RemoveContents<S> where S: event::Stream {

    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

impl<S> event::ElementStream for RemoveContents<S> where S: event::Stream {}

#[derive(Debug)]
struct BuildRemove;

impl<S> transform::BuildOnce<S> for BuildRemove where S: event::Stream {

    type Stream = Remove<S>;

    fn build_once(self, stream: S) -> Self::Stream {
        Remove { stream }
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn remove_contents() {
        test_transform!(
            Default::default(),
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a><b></b>45<c>67</c><b></b></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .remove_contents()
                )
        );
    }

    #[test]
    fn remove_all() {
        test_transform!(
            Default::default(),
            "<a><b>23</b>45<c>67</c><b>89</b></a>",
            "<a>45<c>67</c></a>",
            |html| html
                .select(::select::Tag::from_str("b").unwrap(), |html| html
                    .remove()
                )
        );
    }
}
