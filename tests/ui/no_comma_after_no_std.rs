use dst_factory::make_dst_factory;

#[make_dst_factory(create, no_std deserialize)]
struct NoCommaAfterNoStd {
    id: i32,
    data: str,
}

fn main() {
}
