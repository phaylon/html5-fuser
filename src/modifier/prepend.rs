
use event;
use modifier;
use transform;

#[derive(Debug)]
pub struct Prepend<S, N> {
    stream: modifier::Combine<N, S>,
}

impl<S, N> Prepend<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, prepend: N) -> Prepend<S, N> {
        Prepend {
            stream: modifier::Combine::new(prepend, stream),
        }
    }
}

impl<S, N> event::Stream for Prepend<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

#[derive(Debug)]
pub struct PrependContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    stream: modifier::select::ContentActorOnce<S, BuildPrepend<N>>,
}

impl<S, N> PrependContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, prepend: N) -> PrependContents<S, N> {
        PrependContents {
            stream: modifier::select::ContentActorOnce::new(stream, BuildPrepend {
                stream: prepend,
            }),
        }
    }
}

#[derive(Debug)]
struct BuildPrepend<N> {
    stream: N,
}

impl<S, N> transform::BuildOnce<S> for BuildPrepend<N>
where
    S: event::Stream,
    N: event::Stream,
{
    type Stream = modifier::Combine<N, S>;

    fn build_once(self, stream: S) -> Self::Stream {
        modifier::Combine::new(self.stream, stream)
    }
}

impl<S, N> event::ElementStream for PrependContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{}

impl<S, N> event::Stream for PrependContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    fn next_event(&mut self) -> event::StreamResult {
        self.stream.next_event()
    }
}

#[cfg(test)]
mod tests {
    use std::str::{ FromStr };

    #[test]
    fn prepend_contents() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c><i>foo</i>42</c><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .prepend_contents(&new_template)
                )
        );
    }

    #[test]
    fn prepend() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><i>foo</i><c>42</c><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .prepend(&new_template)
                )
        );
    }
}
