use dst_factory::make_dst_factory;

#[make_dst_factory(create pub)]
struct NoCommaAfterFactoryName {
    id: i32,
    data: str,
}

fn main() {
}
