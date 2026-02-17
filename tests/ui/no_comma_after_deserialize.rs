use dst_factory::make_dst_factory;

#[make_dst_factory(create, deserialize no_std)]
struct NoCommaAfterDeserialize {
    id: i32,
    data: str,
}

fn main() {
}
