use dst_factory::make_dst_factory;

#[make_dst_factory(destructurer = my_destr pub)]
struct NoCommaAfterDestructurer {
    id: i32,
    data: str,
}

fn main() {
}
