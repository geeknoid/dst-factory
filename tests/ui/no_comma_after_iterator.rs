use dst_factory::make_dst_factory;

#[make_dst_factory(iterator = MyIter pub)]
struct NoCommaAfterIterator {
    id: i32,
    elements: [u8],
}

fn main() {
}
