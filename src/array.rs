use anyhow::Result;
use std::ffi::CString;
use std::os::raw::{c_char, c_void};
use std::sync::atomic::AtomicPtr;

/// Wrapper to convert C arrays to Vec
#[allow(dead_code)]
pub fn to_vec<T: Clone>(array: *mut T) -> Result<Vec<T>> {
    unsafe {
        let len = naevc::array_size_rust(array as *const c_void);
        let out = std::slice::from_raw_parts(array, len as usize).to_vec();
        Ok(out)
    }
}

/// Small wrapper for working with C arrays
pub struct Array<T>(AtomicPtr<T>);
impl<T> Default for Array<T> {
    fn default() -> Self {
        Array(AtomicPtr::default())
    }
}
impl<T: Sized> Array<T> {
    pub fn new(vec: &[T]) -> Result<Self> {
        if vec.is_empty() {
            return Ok(Default::default());
        }
        let size = std::mem::size_of::<T>();
        let array =
            unsafe { naevc::array_from_vec(vec.as_ptr() as *const c_void, size, vec.len()) };
        if array.is_null() {
            anyhow::bail!("Failed to create C Array");
        }
        Ok(Array(AtomicPtr::new(array as *mut T)))
    }
    pub fn as_ptr(&self) -> *mut c_void {
        self.0.load(std::sync::atomic::Ordering::Relaxed) as *mut c_void
    }
}
impl<T> Drop for Array<T> {
    fn drop(&mut self) {
        unsafe {
            naevc::_array_free_helper(self.as_ptr());
        }
    }
}

#[derive(Default)]
pub struct ArrayCString {
    data: Vec<AtomicPtr<c_char>>,
    arr: Option<Array<*mut c_char>>,
}
impl ArrayCString {
    pub fn new(vec: &[String]) -> Result<Self> {
        let data: Vec<_> = vec
            .iter()
            .map(|s| {
                let cs = CString::new(s.as_str()).unwrap();
                cs.into_raw()
            })
            .collect();
        let arr = Array::new(&data)?;
        let ptrdata: Vec<_> = data.iter().map(|s| AtomicPtr::new(*s)).collect();
        Ok(ArrayCString {
            data: ptrdata,
            arr: Some(arr),
        })
    }
    pub fn as_ptr(&self) -> *mut *const c_char {
        match &self.arr {
            Some(arr) => arr.as_ptr() as *mut *const c_char,
            None => std::ptr::null_mut(),
        }
    }
}
impl std::fmt::Debug for ArrayCString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "ArrayCString")
    }
}
impl Drop for ArrayCString {
    fn drop(&mut self) {
        for ptr in &self.data {
            unsafe {
                let _ = CString::from_raw(ptr.load(std::sync::atomic::Ordering::Relaxed));
            }
        }
    }
}
