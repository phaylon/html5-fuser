
use event;
use modifier;
use transform;

#[derive(Debug)]
pub struct Append<S, N> {
    stream: modifier::Combine<S, N>,
}

impl<S, N> Append<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, append: N) -> Append<S, N> {
        Append {
            stream: modifier::Combine::new(stream, append),
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

#[derive(Debug)]
pub struct AppendContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    stream: modifier::select::ContentActorOnce<S, BuildAppend<N>>,
}

impl<S, N> AppendContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, append: N) -> AppendContents<S, N> {
        AppendContents {
            stream: modifier::select::ContentActorOnce::new(stream, BuildAppend {
                stream: append,
            }),
        }
    }
}

#[derive(Debug)]
struct BuildAppend<N> {
    stream: N,
}

impl<S, N> transform::BuildOnce<S> for BuildAppend<N>
where
    S: event::Stream,
    N: event::Stream,
{
    type Stream = modifier::Combine<S, N>;

    fn build_once(self, stream: S) -> Self::Stream {
        modifier::Combine::new(stream, self.stream)
    }
}

impl<S, N> event::ElementStream for AppendContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{}

impl<S, N> event::Stream for AppendContents<S, N>
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
    fn append_contents() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>42<i>foo</i></c><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .append_contents(&new_template)
                )
        );
    }

    #[test]
    fn append() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c>42</c><i>foo</i><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .append(&new_template)
                )
        );
    }
}

