
use event;
use modifier;
use transform;

#[derive(Debug)]
pub struct Replace<S, N> {
    stream: modifier::prepend::Prepend<modifier::remove::Remove<S>, N>,
}

impl<S, N> Replace<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, replacement: N) -> Replace<S, N> {
        Replace {
            stream: modifier::prepend::Prepend::new(
                modifier::remove::Remove::new(stream),
                replacement,
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

#[derive(Debug)]
pub struct ReplaceContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    stream: modifier::select::ContentActorOnce<S, BuildReplace<N>>,
}

impl<S, N> ReplaceContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, replacement: N) -> ReplaceContents<S, N> {
        ReplaceContents {
            stream: modifier::select::ContentActorOnce::new(stream, BuildReplace {
                stream: replacement,
            }),
        }
    }
}

#[derive(Debug)]
struct BuildReplace<N> {
    stream: N,
}

impl<S, N> transform::BuildOnce<S> for BuildReplace<N>
where
    S: event::Stream,
    N: event::Stream,
{
    type Stream = modifier::prepend::Prepend<modifier::remove::Remove<S>, N>;

    fn build_once(self, stream: S) -> Self::Stream {
        modifier::prepend::Prepend::new(modifier::remove::Remove::new(stream), self.stream)
    }
}

impl<S, N> event::ElementStream for ReplaceContents<S, N>
where
    S: event::Stream,
    N: event::Stream,
{}

impl<S, N> event::Stream for ReplaceContents<S, N>
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
    fn replace_contents_void() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><link><b>93</b></a>",
            "<a><b>23</b><link><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("link").unwrap(), |html| html
                    .replace_contents(&new_template)
                )
        );
    }

    #[test]
    fn replace_contents_self_closed() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c/><b>93</b></a>",
            "<a><b>23</b><c><i>foo</i></c><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .replace_contents(&new_template)
                )
        );
    }

    #[test]
    fn replace_contents() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><c><i>foo</i></c><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .replace_contents(&new_template)
                )
        );
    }

    #[test]
    fn replace() {
        let new = "<i>foo</i>";
        let new_template = ::Template::from_str(new, Default::default()).unwrap();
        test_transform!(
            Default::default(),
            "<a><b>23</b><c>42</c><b>93</b></a>",
            "<a><b>23</b><i>foo</i><b>93</b></a>",
            |html| html
                .select(::select::Tag::from_str("c").unwrap(), |html| html
                    .replace(&new_template)
                )
        );
    }
}
