//!
//! Rich support to safely create instances of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html).
//!
//! This crate lets you allocate variable data inline at the end of a struct. If you have a
//! struct that gets allocated on the heap and has some variable-length data associated with it
//! (like a string or an array), then you can allocate this data directly inline with the struct.
//! This saves memory by avoiding the need for a pointer and a separate allocation, and saves CPU
//! cycles by eliminating the need for indirection when accessing the data.
//!
//! Rust supports the notion of [Dynamically Sized Types](https://doc.rust-lang.org/reference/dynamically-sized-types.html), known as DSTs,
//! which are types that have a size not known at compile time. DSTs are perfect to implement
//! flexible array members. But unfortunately, Rust doesn't provide an out-of-the-box way to allocate
//! instances of such types. This is where this crate comes in.
//!
//! You can apply the #[[`macro@make_dst_factory`]] attribute to your DST structs, which causes factory
//! functions to be produced that let you easily and safely create instances of your DSTs.
//!
//! ## Why Should You Care?
//!
//! Dynamically sized types aren't for everyone. You can't use them as local variables
//! or put them in arrays or vectors, so they can be inconvenient to use. However, their value
//! lies in situations where you have a lot of heap-allocated objects, as they can substantially
//! reduce the memory footprint of your application. If you're building graphs, trees, or other
//! dynamic data structures, you can often leverage DSTs to keep your individual nodes smaller
//! and more efficient.
//!
//! ## Where to Use DSTs?
//!
//! It can be hard or tedious to discover where in your codebase there re opportunities to use DSTs.
//! You can give the following prompt to your favorite AI to have it find candidate structs that
//! can be upgraded to a DST.
//!
//! ```text
//! Analyze this Rust codebase for DST (Dynamically Sized Type) optimization opportunities using the dst_factory crate pattern.
//!
//! A DST struct has one trailing unsized field that is co-located with the struct header in a single allocation (behind Arc, Box, or Rc), eliminating one heap indirection. There are three forms:
//!
//!  - String field → trailing str (eliminates the String's heap buffer)
//!  - Vec<T> field → trailing [T] (eliminates the Vec's heap buffer)
//!  - Box<dyn Trait> field → trailing dyn Trait (eliminates the Box's heap allocation and pointer indirection)
//!
//! A struct is a candidate if ALL of these are true:
//!
//!  1. It has at least one String, Vec<T>, or Box<dyn Trait> field
//!  2. It is used behind Arc<T>, Box<T>, or Rc<T> — NOT stored inline in a Vec, HashMap, array, or by value on the stack
//!  3. The candidate field is not resized, replaced, or swapped after construction. In-place mutation of elements (e.g., writing to [u8] slots, calling &mut self methods on a dyn Trait) is fine — only
//! operations that change the length or swap the entire value are disqualifying (e.g., push(), pop(), clear(), resize(), reassigning the field to a new String/Vec/Box<dyn>)
//!
//! A struct with multiple candidate fields is still a valid candidate — the user picks one field as the trailing unsized tail, the others stay as-is. When listing candidates, note ALL eligible tail fields and
//! recommend which one to pick (typically the largest or most frequently populated).
//!
//! For each crate, systematically:
//!
//!  1. Find all struct definitions (both pub and private) that have String, Vec<T>, or Box<dyn Trait> fields
//!  2. For each, grep for Arc<StructName>, Box<StructName>, Vec<StructName> to determine usage pattern
//!  3. Check whether candidate fields are resized, replaced, or swapped after construction (in-place element mutation is OK)
//!  4. Report: file path, full struct definition, all candidate tail fields, usage pattern (Arc/Box/Vec/value), mutability assessment, and recommended tail choice
//!
//! Exclude:
//!
//!  - Structs stored in Vec<T> or HashMap values (DSTs are !Sized)
//!  - Structs where the candidate field is resized or replaced after construction (e.g., push(), clear(), field reassignment)
//!  - Structs not behind Arc/Box/Rc (no allocation to optimize)
//!  - Test-only structs
//!
//! Output format per candidate:
//!
//!  ### StructName
//!  File: path/to/file.rs:line
//!  Arc/Box usage: N sites (list key files)
//!  Candidate tail fields:
//!    - field_name: Type → unsized_type (recommended: yes/no, reason)
//!    - field_name: Type → unsized_type
//!  Resized/replaced after construction: no (cite evidence) / yes (disqualifying method)
//!  Volume: how often constructed per request/operation
//!  Savings: 1 allocation per instance × volume
//! ```
//!
//! ## Examples
//!
//! Here's an example using an array as the last field of a struct:
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory]
//! struct User {
//!     age: u32,
//!     signing_key: [u8],
//! }
//!
//! // allocate one user with a 4-byte key
//! let a = User::build(33, [0, 1, 2, 3]);
//!
//! // allocate another user with a 5-byte key
//! let b = User::build_from_slice(33, &[0, 1, 2, 3, 4]);
//!
//! // allocate another user, this time using an iterator
//! let v = vec![0, 1, 2, 3, 4];
//! let c = User::build(33, v.iter().copied());
//!
//! // destructure this user and compare its key to the vector
//! // this has the advantage of iterating over u8, not &u8 or &mut u8.
//! let (_age, signing_key) = User::destructure(c);
//! assert!(signing_key.eq(v.into_iter()));
//! ```
//! Here's another example, this time using a string as the last field of a struct:
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory]
//! struct User {
//!     age: u32,
//!     name: str,
//! }
//!
//! // allocate one user with a 5-character string
//! let a = User::build(33, "Alice");
//!
//! // allocate another user with a 3-character string
//! let b = User::build(33, "Bob");
//! ```
//! And finally, here's an example using a trait object as the last field of a struct:
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! // a trait we'll use in our DST
//! trait NumberProducer {
//!     fn get_number(&self) -> u32;
//! }
//!
//! // an implementation of the trait we're going to use
//! struct FortyTwoProducer;
//! impl NumberProducer for FortyTwoProducer {
//!     fn get_number(&self) -> u32 {
//!         42
//!     }
//! }
//!
//! // another implementation of the trait we're going to use
//! struct TenProducer;
//! impl NumberProducer for TenProducer {
//!     fn get_number(&self) -> u32 {
//!         10
//!     }
//! }
//!
//! #[make_dst_factory]
//! struct Node {
//!     count: u32,
//!     producer: dyn NumberProducer,
//! }
//!
//! // allocate an instance with one implementation of the trait
//! let a = Node::build(33, FortyTwoProducer{});
//! assert_eq!(42, a.producer.get_number());
//!
//! // allocate an instance with another implementation of the trait
//! let b = Node::build(33, TenProducer{});
//! assert_eq!(10, b.producer.get_number());
//! ```
//!
//! Because DSTs don't have a known size at compile time, you can't store them on the stack,
//! and you can't pass them by value. As a result of these constraints, the factory functions
//! return smart-pointer-wrapped instances of the structs.
//!
//! ## Smart Pointers
//!
//! The macro generates factory functions for three smart pointer types, each constructed
//! in a **single allocation**:
//!
//! | Pointer | Factory suffix | Use case |
//! |---------|---------------|----------|
//! | [`Box<T>`] | *(none)* | Unique ownership (default) |
//! | [`Arc<T>`](std::sync::Arc) | `_arc` | Shared ownership, thread-safe (atomic refcount) |
//! | [`Rc<T>`](std::rc::Rc) | `_rc` | Shared ownership, single-threaded (non-atomic refcount) |
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//! use std::sync::Arc;
//! use std::rc::Rc;
//!
//! #[make_dst_factory]
//! struct User {
//!     age: u32,
//!     name: str,
//! }
//!
//! // Unique ownership
//! let boxed: Box<User> = User::build(33, "Alice");
//!
//! // Thread-safe shared ownership
//! let shared: Arc<User> = User::build_arc(33, "Bob");
//! let clone = Arc::clone(&shared);
//! assert_eq!(&clone.name, "Bob");
//!
//! // Single-threaded shared ownership
//! let local: Rc<User> = User::build_rc(33, "Carol");
//! let clone = Rc::clone(&local);
//! assert_eq!(&clone.name, "Carol");
//! ```
//!
//! ## Attribute Features
//!
//! The common use case for the #[[`macro@make_dst_factory`]] attribute is to not pass any arguments.
//! This results in functions called `build`, `build_arc`, and `build_rc` when using a string
//! or dynamic trait as the last field of the struct, and additionally `build_from_slice`,
//! `build_arc_from_slice`, `build_rc_from_slice`, and `destructure` when using an array as the
//! last field of the struct.
//!
//! The generated functions are private by default and have the following signatures:
//!
//! ```ignore
//! // for arrays
//! fn build<G>(field1, field2, ..., last_field: G) -> Box<Self>
//! where
//!     G: IntoIterator<Item = last_field_type>,
//!     <G as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! fn build_from_slice(field1, field2, ..., last_field: &[last_field_type]) -> Box<Self>
//! where
//!     last_field_type: Copy + Sized;
//!
//! fn build_arc<G>(field1, field2, ..., last_field: G) -> Arc<Self>
//! where
//!     G: IntoIterator<Item = last_field_type>,
//!     <G as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! fn build_arc_from_slice(field1, field2, ..., last_field: &[last_field_type]) -> Arc<Self>
//! where
//!     last_field_type: Copy + Sized;
//!
//! fn build_rc<G>(field1, field2, ..., last_field: G) -> Rc<Self>
//! where
//!     G: IntoIterator<Item = last_field_type>,
//!     <G as IntoIterator>::IntoIter: ExactSizeIterator,
//!
//! fn build_rc_from_slice(field1, field2, ..., last_field: &[last_field_type]) -> Rc<Self>
//! where
//!     last_field_type: Copy + Sized;
//!
//! fn destructure(this: Box<Self>) -> (Type1, Type2, ..., SelfIter);
//!
//! // for strings
//! fn build(field1, field2, ..., last_field: impl AsRef<str>) -> Box<Self>;
//! fn build_arc(field1, field2, ..., last_field: impl AsRef<str>) -> Arc<Self>;
//! fn build_rc(field1, field2, ..., last_field: impl AsRef<str>) -> Rc<Self>;
//!
//! // for traits
//! fn build(field1, field2, ..., last_field: G) -> Box<Self>
//! where
//!     G: TraitName + Sized;
//!
//! fn build_arc(field1, field2, ..., last_field: G) -> Arc<Self>
//! where
//!     G: TraitName + Sized;
//!
//! fn build_rc(field1, field2, ..., last_field: G) -> Rc<Self>
//! where
//!     G: TraitName + Sized;
//! ```
//!
//! The attribute lets you control the name of the generated functions, their
//! visibility, and whether to generate code for the `no_std` environment. The general
//! grammar is:
//!
//! ```ignore
//! #[make_dst_factory(<base_factory_name> [, destructurer=<destructurer_name>] [, iterator=<iterator_name>] [, <visibility>] [, no_std] [, deserialize] [, clone] [, generic=<generic_name>])]
//! ```
//!
//! Some examples:
//!
//! ```ignore
//! // Public factories: build, build_from_slice, build_arc, build_arc_from_slice,
//! // build_rc, build_rc_from_slice, and destructure.
//! #[make_dst_factory(pub)]
//!
//! // Custom base name: create, create_from_slice, create_arc, create_arc_from_slice,
//! // create_rc, create_rc_from_slice, and destructure.
//! #[make_dst_factory(create)]
//!
//! // Custom destructurer name.
//! #[make_dst_factory(create, destructurer = destroy)]
//!
//! // Public with custom name.
//! #[make_dst_factory(create, pub)]
//!
//! // Support the `no_std` environment.
//! #[make_dst_factory(create, no_std)]
//!
//! // Custom generic type name.
//! #[make_dst_factory(create, no_std, generic=X)]
//! ```
//!
//! ## Other Features
//!
//! You can use the #[[`macro@make_dst_factory`]] attribute on structs with the normal Rust
//! representation or C representation (`#[repr(C)]`), with any padding and alignment
//! specification. See the Rust reference on [Type Layout](https://doc.rust-lang.org/reference/type-layout.html)
//! for more details.
//!
//! ## Clone Support
//!
//! Because DST structs are unsized, `#[derive(Clone)]` cannot work for `Box<T>` since the
//! standard derive doesn't know how to reconstruct the struct. Passing the `clone` flag in
//! the attribute generates a `Clone` implementation for `Box<T>` that uses the macro-generated
//! factory functions to create a deep copy.
//!
//! ```rust
//! use dst_factory::make_dst_factory;
//!
//! #[make_dst_factory(clone)]
//! struct Message {
//!     id: u32,
//!     text: str,
//! }
//!
//! let msg = Message::build(1, "hello");
//! let cloned = msg.clone();
//! assert_eq!(cloned.id, 1);
//! assert_eq!(&cloned.text, "hello");
//! ```
//!
//! Clone support works with both named and tuple structs, and with both `str` and `[T]`
//! slice tails. It is not supported for `dyn Trait` tails, since there is no way to
//! clone the concrete type through a trait object reference.
//!
//! ## Serde Support
//!
//! DST structs work naturally with `#[derive(Serialize)]` from serde, since serialization
//! only requires a reference. However, `#[derive(Deserialize)]` cannot work because the
//! standard derive tries to construct the struct directly, which is impossible for unsized types.
//!
//! Passing the `deserialize` flag in the attribute generates a
//! [`Deserialize`](https://docs.rs/serde/latest/serde/trait.Deserialize.html) implementation
//! for `Box<T>` that uses the macro-generated factory functions to construct the struct.
//! All standard `#[serde(...)]` field attributes (such as `rename`, `default`, `skip`, etc.)
//! are fully supported.
//!
//! ```ignore
//! use dst_factory::make_dst_factory;
//! use serde::Serialize;
//!
//! #[derive(Serialize)]
//! #[make_dst_factory(deserialize)]
//! struct Message {
//!     id: u32,
//!     text: str,
//! }
//!
//! // Serialize
//! let msg = Message::build(1, "hello");
//! let json = serde_json::to_string(&*msg).unwrap();
//!
//! // Deserialize
//! let restored: Box<Message> = serde_json::from_str(&json).unwrap();
//! assert_eq!(restored.id, 1);
//! assert_eq!(&restored.text, "hello");
//! ```
//!
//! ### Deserializing into `Arc` and `Rc`
//!
//! Rust's orphan rules prevent implementing `Deserialize` for `Arc<T>` or `Rc<T>` directly
//! (they are not `#[fundamental]` like `Box<T>`). Instead, the `deserialize` flag generates
//! helper functions `deserialize_arc` and `deserialize_rc` that can be used with serde's
//! `#[serde(deserialize_with = "...")]` attribute:
//!
//! ```ignore
//! use dst_factory::make_dst_factory;
//! use serde::{Serialize, Deserialize};
//! use std::sync::Arc;
//! use std::rc::Rc;
//!
//! #[derive(Serialize)]
//! #[make_dst_factory(deserialize)]
//! struct Message {
//!     id: u32,
//!     text: str,
//! }
//!
//! #[derive(Serialize, Deserialize)]
//! struct Dashboard {
//!     #[serde(deserialize_with = "Message::deserialize_arc")]
//!     shared_msg: Arc<Message>,
//!
//!     #[serde(deserialize_with = "Message::deserialize_rc")]
//!     local_msg: Rc<Message>,
//! }
//! ```
//!
//! Serde support works with both named and tuple structs, and with both `str` and `[T]`
//! slice tails. It is not supported for `dyn Trait` tails, since there is no way to
//! reconstruct the concrete type from serialized data.
//!
//! ## Error Conditions
//!
//! The #[[`macro@make_dst_factory`]] attribute produces a compile-time error if:
//!
//! - It's applied to anything other than a regular struct or a tuple struct.
//! - Its arguments are malformed (e.g., incorrect visibility keyword, too many arguments).
//! - The struct has no fields.
//! - The last field of the struct is not a slice (`[T]`), a string (`str`), or a trait object (`dyn Trait`).
//! - The resulting struct exceeds the maximum size allowed of `isize::MAX`.
//! - The `deserialize` flag is used on a struct with a `dyn Trait` tail.
//! - The `clone` flag is used on a struct with a `dyn Trait` tail.
//!
//! ## Acknowledgments
//!
//! Many thanks to <https://github.com/scottmcm> for his invaluable help getting the factory methods
//! in top shape.

mod r#box;
mod common;
mod macro_args;
mod serde;

/// Generate factory functions for dynamically sized types (DST) structs.
///
/// Refer to the [crate-level documentation](crate) for more details and example uses.
#[proc_macro_attribute]
pub fn make_dst_factory(attr_args: proc_macro::TokenStream, item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let result = common::make_dst_factory_impl(attr_args.into(), item.into());
    result.unwrap_or_else(|err| err.to_compile_error()).into()
}
