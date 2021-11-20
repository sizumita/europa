use std::ffi::CStr;
use std::os::raw::{c_char, c_int, c_long};


#[no_mangle] pub extern "C" fn println(x: *mut c_char) -> i32 {
    let x_slice = unsafe {CStr::from_ptr(x)};
    println!("{}", x_slice.to_str().unwrap());
    0
}

#[no_mangle] pub extern "C" fn printi(x: i32) -> i32 {
    println!("{}", x);
    0
}

#[no_mangle] pub extern "C" fn __String__compare(x: *mut c_char, y: *mut c_char) -> c_int {
    let r = unsafe {libc::strcmp(x, y)};
    r
}
