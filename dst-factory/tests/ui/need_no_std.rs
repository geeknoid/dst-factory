use dst_factory::make_dst_factory;

#[make_dst_factory(create, pub, X)]
struct NeedNoStd {
    id: i32,
    data: str,
}

fn main() {
}
