use tail_extend::{tail_attr, tail_extend};

// this will generate a struct called User with a "name" field.
#[tail_extend(User, pub name: str)]
#[tail_attr(derive(Debug))]
struct UserArgs {
    age : u8,
}

// This will generate a struct called Credential with a "key" field.
#[tail_extend(Credential, key: [u8])]
#[tail_attr(derive(Debug))]
struct CredentialArgs {
    ttl: u32,
}

fn main() {
    // Create a user instance.
    let user: Box<User> = User::build(UserArgs { age: 33 }, "A string!");
    println!("User name: {}", &user.name);
    println!("User age : {}", user.age);

    // Create a Credential instance.
    let cred: Box<Credential> = Credential::build(CredentialArgs { ttl: 33 }, &[0, 1, 2, 3]);
    println!("TTL: {}", &cred.ttl);
    println!("Key: {:?}", &cred.key);
}
