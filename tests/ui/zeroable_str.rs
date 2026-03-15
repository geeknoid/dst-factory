use dst_factory::make_dst_factory;

#[make_dst_factory(zeroable)]
struct ZeroableStr {
    id: i32,
    data: str,
}

fn main() {
}
