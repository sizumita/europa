use std::ffi::CStr;
use std::os::raw::{c_char, c_int};

#[no_mangle] pub extern "C" fn __String__compare(x: *mut c_char, y: *mut c_char) -> c_int {
    let r = unsafe {libc::strcmp(x, y)};
    r
}

#[no_mangle] pub extern "C" fn __String__equals(x: *mut c_char, y: *mut c_char) -> c_int {
    let r = unsafe {libc::strcmp(x, y)};
    c_int::from(r.eq(&0))
}

#[no_mangle] pub extern "C" fn __String__length(x: *mut c_char) -> c_int {
    let x_slice = unsafe {CStr::from_ptr(x)};
    c_int::from(x_slice.to_str().unwrap().len() as i32)
}
