use dst_factory::make_dst_factory;

#[repr(C)]
struct User {
    age: u8,
    name: str,
}
impl User {
    ///Creates an instance of `Box<User>` from a string slice.
    #[allow(clippy::let_unit_value)]
    #[allow(clippy::zst_offset)]
    #[allow(clippy::transmute_undefined_repr)]
    pub(crate) fn new(
        age: u8,
        name: impl ::core::convert::AsRef<str>,
    ) -> ::std::boxed::Box<Self> {
        let a = (age, name);
        let s = a.1.as_ref();
        let len = s.len();
        let layout = ::core::alloc::Layout::from_size_align(0, 1).unwrap();
        let layout = layout.extend(::core::alloc::Layout::new::<u8>()).unwrap().0;
        let layout = layout
            .extend(
                ::core::alloc::Layout::array::<u8>(len)
                    .expect("Array exceeds maximum size allowed of isize::MAX"),
            )
            .expect("Struct exceeds maximum size allowed of isize::MAX")
            .0;
        let layout = layout.pad_to_align();
        unsafe {
            if layout.size() == 0 {
                let mem_ptr = ::core::ptr::NonNull::<()>::dangling().as_ptr();
                let fat_ptr = ::core::mem::transmute::<
                    (*mut (), usize),
                    *mut Self,
                >((mem_ptr, len));
                ::std::boxed::Box::from_raw(fat_ptr)
            } else {
                let mem_ptr = ::std::alloc::alloc(layout);
                if mem_ptr.is_null() {
                    ::std::alloc::handle_alloc_error(layout)
                }
                let fat_ptr = ::core::mem::transmute::<
                    (*mut u8, usize),
                    *mut Self,
                >((mem_ptr, len));
                ::core::ptr::write(&mut ((*fat_ptr).age), a.0);
                let tail_ptr = (&raw mut (*fat_ptr).name).cast::<u8>();
                ::core::ptr::copy_nonoverlapping(s.as_ptr(), tail_ptr, len);
                ::std::boxed::Box::from_raw(fat_ptr)
            }
        }
    }
}
#[repr(C)]
struct Credential {
    ttl: u32,
    key: [u16],
}
impl Credential {
    ///Creates an instance of `Box<Credential>` from a slice.
    #[allow(clippy::let_unit_value)]
    #[allow(clippy::zst_offset)]
    #[allow(clippy::transmute_undefined_repr)]
    fn build(ttl: u32, key: &[u16]) -> ::std::boxed::Box<Self>
    where
        u16: ::core::clone::Clone,
    {
        let a = (ttl, key);
        let len = a.1.len();
        let layout = ::core::alloc::Layout::from_size_align(0, 1).unwrap();
        let layout = layout.extend(::core::alloc::Layout::new::<u32>()).unwrap().0;
        let layout = layout
            .extend(
                ::core::alloc::Layout::array::<u16>(len)
                    .expect("Array exceeds maximum size allowed of isize::MAX"),
            )
            .expect("Struct exceeds maximum size allowed of isize::MAX")
            .0;
        let layout = layout.pad_to_align();
        unsafe {
            if layout.size() == 0 {
                let mem_ptr = ::core::ptr::NonNull::<()>::dangling().as_ptr();
                let fat_ptr = ::core::mem::transmute::<
                    (*mut (), usize),
                    *mut Self,
                >((mem_ptr, len));
                ::std::boxed::Box::from_raw(fat_ptr)
            } else {
                let mem_ptr = ::std::alloc::alloc(layout);
                if mem_ptr.is_null() {
                    ::std::alloc::handle_alloc_error(layout)
                }
                let fat_ptr = ::core::mem::transmute::<
                    (*mut u8, usize),
                    *mut Self,
                >((mem_ptr, len));
                ::core::ptr::write(&mut ((*fat_ptr).ttl), a.0);
                let tail_ptr = (&raw mut (*fat_ptr).key).cast::<u16>();
                ::core::ptr::copy_nonoverlapping(key.as_ptr(), tail_ptr, len);
                ::std::boxed::Box::from_raw(fat_ptr)
            }
        }
    }
    ///Creates an instance of `Box<Credential>` from an iterator.
    #[allow(clippy::let_unit_value)]
    #[allow(clippy::zst_offset)]
    #[allow(clippy::transmute_undefined_repr)]
    fn build_from_iter<I>(ttl: u32, key: I) -> ::std::boxed::Box<Self>
    where
        I: ::core::iter::IntoIterator<Item = u16>,
        <I as ::core::iter::IntoIterator>::IntoIter: ::core::iter::ExactSizeIterator,
    {
        struct Guard<T> {
            mem_ptr: *mut u8,
            tail_ptr: *mut T,
            initialized: usize,
            layout: ::core::alloc::Layout,
        }
        impl<T> Drop for Guard<T> {
            fn drop(&mut self) {
                unsafe {
                    for i in 0..self.initialized {
                        ::core::ptr::drop_in_place(self.tail_ptr.add(i));
                    }
                    ::std::alloc::dealloc(self.mem_ptr, self.layout);
                }
            }
        }
        let a = (ttl, key);
        let iter = a.1.into_iter();
        let len = iter.len();
        let layout = ::core::alloc::Layout::from_size_align(0, 1).unwrap();
        let layout = layout.extend(::core::alloc::Layout::new::<u32>()).unwrap().0;
        let layout = layout
            .extend(
                ::core::alloc::Layout::array::<u16>(len)
                    .expect("Array exceeds maximum size allowed of isize::MAX"),
            )
            .expect("Struct exceeds maximum size allowed of isize::MAX")
            .0;
        let layout = layout.pad_to_align();
        unsafe {
            if layout.size() == 0 {
                let mem_ptr = ::core::ptr::NonNull::<()>::dangling().as_ptr();
                let fat_ptr = ::core::mem::transmute::<
                    (*mut (), usize),
                    *mut Self,
                >((mem_ptr, len));
                ::std::boxed::Box::from_raw(fat_ptr)
            } else {
                let mem_ptr = ::std::alloc::alloc(layout);
                if mem_ptr.is_null() {
                    ::std::alloc::handle_alloc_error(layout)
                }
                let fat_ptr = ::core::mem::transmute::<
                    (*mut u8, usize),
                    *mut Self,
                >((mem_ptr, len));
                ::core::ptr::write(&mut ((*fat_ptr).ttl), a.0);
                let tail_ptr = (&raw mut (*fat_ptr).key).cast::<u16>();
                let mut guard = Guard {
                    mem_ptr,
                    tail_ptr,
                    layout,
                    initialized: 0,
                };
                for (i, element) in iter.enumerate() {
                    ::core::ptr::write(tail_ptr.add(i), element);
                    guard.initialized += 1;
                }
                ::std::mem::forget(guard);
                ::std::boxed::Box::from_raw(fat_ptr)
            }
        }
    }
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
