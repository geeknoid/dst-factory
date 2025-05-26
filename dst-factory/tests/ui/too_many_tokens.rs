use dst_factory::make_dst_factory;

#[make_dst_factory(no_std, XXX)]
struct ToManyTokens {
    id: i32,
    data: str,
}

fn main() {
}
