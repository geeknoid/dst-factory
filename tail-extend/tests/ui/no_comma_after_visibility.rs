use tail_extend::make_dst_builder;

#[make_dst_builder(create, pub no_std)]
struct NoCommaAfterVisibility {
    id: i32,
    data: str,
}

fn main() {
}
