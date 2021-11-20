mod string;

use std::ffi::CStr;
use std::os::raw::{c_char, c_int, c_long, c_void};

#[no_mangle] pub extern "C" fn println(x: *mut c_char) -> c_int {
    let x_slice = unsafe {CStr::from_ptr(x)};
    println!("{}", x_slice.to_str().unwrap());
    c_int::from(0)
}

#[no_mangle] pub extern "C" fn printi(x: i32) -> c_int {
    println!("{}", x);
    c_int::from(0)
}
