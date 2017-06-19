
use event;
use modifier;
use transform;
use builder;

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

