#![allow(dead_code)]
use std::ffi::c_void;

pub struct SystemWrapper(naevc::StarSystem);
unsafe impl Send for SystemWrapper {}

impl SystemWrapper {
   pub fn presence(&self) -> &[naevc::SystemPresence] {
      let presences = self.0.presence;
      unsafe {
         let n = naevc::array_size_rust(presences as *const c_void) as usize;
         std::slice::from_raw_parts(presences as *const naevc::SystemPresence, n)
      }
   }

   pub fn presence_mut(&mut self) -> &mut [naevc::SystemPresence] {
      let presences = self.0.presence;
      unsafe {
         let n = naevc::array_size_rust(presences as *const c_void) as usize;
         std::slice::from_raw_parts_mut(presences, n)
      }
   }
}

pub fn get() -> &'static [SystemWrapper] {
   unsafe {
      let systems = naevc::system_getAll();
      let n = naevc::array_size_rust(systems as *const c_void) as usize;
      std::slice::from_raw_parts(systems as *const SystemWrapper, n)
   }
}

pub fn get_mut() -> &'static mut [SystemWrapper] {
   unsafe {
      let systems = naevc::system_getAll();
      let n = naevc::array_size_rust(systems as *const c_void) as usize;
      std::slice::from_raw_parts_mut(systems as *mut SystemWrapper, n)
   }
}
