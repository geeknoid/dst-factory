use dst_factory::make_dst_factory;

#[make_dst_factory]
struct MultipleDstFields {
    id: str,
    data: str,
}

fn main() {
}
