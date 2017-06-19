
use event;
use modifier;
use transform;
use builder;

pub struct Prepend<S, N> {
    stream: modifier::Combine<N, S>,
}

impl<S, N> Prepend<S, N>
where
    S: event::Stream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> Prepend<S, N> {
        Prepend {
            stream: modifier::Combine::new(new_stream, stream),
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

pub struct PrependContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    stream: modifier::select::SelectContent<S, Builder<N>>,
}

impl<S, N> PrependContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{
    pub(crate) fn new(stream: S, new_stream: N) -> PrependContent<S, N> {
        PrependContent {
            stream: modifier::select::SelectContent::new(stream, Builder { new_stream }),
        }
    }
}

impl<S, N> event::ElementStream for PrependContent<S, N>
where
    S: event::ElementStream,
    N: event::Stream,
{}

impl<S, N> event::Stream for PrependContent<S, N>
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
    type Stream = Prepend<modifier::select::CurrentContent<S>, N>;

    fn build_once(self, stream: transform::Api<modifier::select::CurrentContent<S>>)
    -> transform::Api<Self::Stream> {
        transform::Api::pack(Prepend::new(stream.unpack(), self.new_stream))
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
