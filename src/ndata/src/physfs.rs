/* Documentation mentions global lock in settings. Should be thread-safe _except_ for opening the
 * same file and writing + reading/writing with multiple threads. */
use sdl3 as sdl;
use std::ffi::{CStr, CString};
use std::io::{Error, Read, Result, Seek, SeekFrom, Write};
use std::os::raw::c_void;
use std::sync::atomic::{AtomicPtr, Ordering};

// Some stuff is based on the physfs-rs package.
// Modified to not use a global context and use functions from naevc
pub fn error_as_io_error(func: &str) -> Error {
    let cerrstr = unsafe {
        CStr::from_ptr(naevc::PHYSFS_getErrorByCode(
            naevc::PHYSFS_getLastErrorCode(),
        ))
    };
    Error::other(format!(
        "PhysicsFS Error with '{}': `{}`",
        func,
        cerrstr.to_str().unwrap_or("Unknown")
    ))
}

pub fn error_as_io_error_with_file(func: &str, file: &str) -> Error {
    let cerrstr = unsafe {
        CStr::from_ptr(naevc::PHYSFS_getErrorByCode(
            naevc::PHYSFS_getLastErrorCode(),
        ))
    };
    Error::other(format!(
        "PhysicsFS Error with '{}' on file '{}': `{}`",
        func,
        file,
        cerrstr.to_str().unwrap_or("Unknown")
    ))
}

/// Possible ways to open a file.
#[allow(dead_code)]
#[derive(Copy, Clone)]
pub enum Mode {
    /// Append to the end of the file.
    Append,
    /// Read from the file.
    Read,
    /// Write to the file, overwriting previous data.
    Write,
}

/// A file handle.
pub struct File<'f> {
    raw: AtomicPtr<naevc::PHYSFS_File>,
    //mode: Mode,
    _marker: std::marker::PhantomData<&'f isize>,
}

impl File<'_> {
    /// Opens a file with a specific mode.
    pub fn open<'g>(filename: &str, mode: Mode) -> Result<File<'g>> {
        let c_filename = CString::new(filename)?;
        let raw = unsafe {
            match mode {
                Mode::Append => naevc::PHYSFS_openAppend(c_filename.as_ptr()),
                Mode::Read => naevc::PHYSFS_openRead(c_filename.as_ptr()),
                Mode::Write => naevc::PHYSFS_openWrite(c_filename.as_ptr()),
            }
        };

        if raw.is_null() {
            Err(error_as_io_error_with_file("PHYSFS_open", filename))
        } else {
            Ok(File {
                raw: AtomicPtr::new(raw),
                //mode,
                _marker: std::marker::PhantomData,
            })
        }
    }

    /// Closes a file handle.
    fn close(&self) -> Result<()> {
        match unsafe { naevc::PHYSFS_close(self.raw.load(Ordering::Relaxed)) } {
            0 => Err(error_as_io_error("PHYSFS_close")),
            _ => Ok(()),
        }
    }

    /// Checks whether eof is reached or not.
    #[allow(dead_code)]
    pub fn eof(&self) -> bool {
        let ret = unsafe { naevc::PHYSFS_eof(self.raw.load(Ordering::Relaxed)) };
        ret != 0
    }

    /// Determine length of file, if possible
    pub fn len(&self) -> Result<u64> {
        let len = unsafe { naevc::PHYSFS_fileLength(self.raw.load(Ordering::Relaxed)) };
        if len >= 0 {
            Ok(len as u64)
        } else {
            Err(error_as_io_error("PHYSFS_fileLength"))
        }
    }

    /// Determine if file is empty
    pub fn is_empty(&self) -> bool {
        match self.len() {
            Ok(len) => len > 0,
            Err(_) => true,
        }
    }

    /// Determines current position within a file
    pub fn tell(&self) -> Result<u64> {
        let ret = unsafe { naevc::PHYSFS_tell(self.raw.load(Ordering::Relaxed)) };
        match ret {
            -1 => Err(error_as_io_error("PHYSFS_tell")),
            _ => Ok(ret as u64),
        }
    }
}

impl Read for File<'_> {
    /// Reads from a file
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let ret = unsafe {
            naevc::PHYSFS_readBytes(
                self.raw.load(Ordering::Relaxed),
                buf.as_ptr() as *mut c_void,
                buf.len() as naevc::PHYSFS_uint64,
            )
        };
        match ret {
            -1 => Err(error_as_io_error("PHYSFS_readBytes")),
            _ => Ok(ret as usize),
        }
    }
}

impl Write for File<'_> {
    /// Writes to a file.
    /// This code performs no safety checks to ensure
    /// that the buffer is the correct length.
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        let ret = unsafe {
            naevc::PHYSFS_writeBytes(
                self.raw.load(Ordering::Relaxed),
                buf.as_ptr() as *const c_void,
                buf.len() as naevc::PHYSFS_uint64,
            )
        };

        match ret {
            -1 => Err(error_as_io_error("PHYSFS_writeBytes")),
            _ => Ok(ret as usize),
        }
    }

    /// Flushes a file if buffered; no-op if unbuffered.
    fn flush(&mut self) -> Result<()> {
        let ret = unsafe { naevc::PHYSFS_flush(self.raw.load(Ordering::Relaxed)) };
        match ret {
            0 => Err(error_as_io_error("PHYSFS_flush")),
            _ => Ok(()),
        }
    }
}

impl Seek for File<'_> {
    /// Seek to a new position within a file
    fn seek(&mut self, pos: SeekFrom) -> Result<u64> {
        let seek_pos = match pos {
            SeekFrom::Start(n) => n as i64,
            SeekFrom::End(n) => {
                let len = self.len()?;
                n + len as i64
            }
            SeekFrom::Current(n) => {
                let curr_pos = self.tell()?;
                n + curr_pos as i64
            }
        };
        let result = unsafe {
            naevc::PHYSFS_seek(
                self.raw.load(Ordering::Relaxed),
                seek_pos as naevc::PHYSFS_uint64,
            )
        };
        if result == -1 {
            return Err(error_as_io_error("PHYSFS_seek"));
        }
        self.tell()
    }
}

impl Drop for File<'_> {
    fn drop(&mut self) {
        let _ = self.close();
    }
}

pub fn read_dir(path: &str) -> Result<Vec<String>> {
    let c_path = CString::new(path)?;
    let mut list = unsafe { naevc::PHYSFS_enumerateFiles(c_path.as_ptr()) };
    if list.is_null() {
        return Err(error_as_io_error_with_file("PHYSFS_enumerateFiles", path));
    }
    let listptr = list;

    let mut res = vec![];
    while !unsafe { *list }.is_null() {
        unsafe {
            let filename = format!("{}/{}", path, CStr::from_ptr(*list).to_str().unwrap());
            res.push(filename);
        }
        list = ((list as usize) + std::mem::size_of_val(&list)) as _;
    }
    unsafe {
        naevc::PHYSFS_freeList(listptr as *mut c_void);
    }

    Ok(res)
}

pub fn iostream(filename: &str, mode: Mode) -> Result<sdl::iostream::IOStream> {
    let raw = unsafe {
        let c_filename = CString::new(filename)?;
        let phys = match mode {
            Mode::Append => naevc::PHYSFS_openAppend(c_filename.as_ptr()),
            Mode::Read => naevc::PHYSFS_openRead(c_filename.as_ptr()),
            Mode::Write => naevc::PHYSFS_openWrite(c_filename.as_ptr()),
        };
        if phys.is_null() {
            return Err(error_as_io_error_with_file("PHYSFS_open", filename));
        }
        naevc::SDL_PhysFS_OpenIO(phys)
    };
    if raw.is_null() {
        Err(error_as_io_error_with_file("PHYSFS_open", filename))
    } else {
        Ok(unsafe {
            sdl::iostream::IOStream::from_ll(raw as *mut sdl::sys::iostream::SDL_IOStream)
        })
    }
}

pub fn blacklisted(filename: &str) -> bool {
    let c_filename = CString::new(filename).unwrap();
    let realdir = unsafe { naevc::PHYSFS_getRealDir(c_filename.as_ptr()) };
    if realdir.is_null() {
        return false;
    }
    let realdir = unsafe { CStr::from_ptr(realdir) };
    realdir.to_str().unwrap() == "naev.BLACKLIST"
}
