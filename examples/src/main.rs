use tail_extend::make_dst_builder;

#[make_dst_builder(pub(crate) new)]
struct User {
    age: u8,
    name: str,
}

#[make_dst_builder]
struct Credential {
    ttl: u32,
    key: [u8],
}

fn main() {
    // Create a user instance.
    let user: Box<User> = User::new(33, "A string!");
    println!("User name: {}", &user.name);
    println!("User age : {}", user.age);

    // Create a Credential instance.
    let cred: Box<Credential> = Credential::build(60, &[0, 1, 2, 3]);
    println!("TTL: {}", &cred.ttl);
    println!("Key: {:?}", &cred.key);
}
