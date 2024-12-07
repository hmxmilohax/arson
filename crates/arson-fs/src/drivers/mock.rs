// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    collections::HashMap,
    io::{self, Read, Write},
    rc::Rc,
    time::SystemTime,
};

use crate::{AbsolutePath, FileSystemDriver, Metadata, ReadWrite};

#[derive(Debug, Clone)]
struct MockFile {
    bytes: Rc<Vec<u8>>,
    position: usize,
}

impl MockFile {
    fn new(bytes: Rc<Vec<u8>>) -> Self {
        Self { bytes, position: 0 }
    }
}

impl Read for MockFile {
    fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        let Some(bytes) = self.bytes.get(self.position..) else {
            return Ok(0);
        };

        let count = bytes.len().min(buf.len());
        let src = &bytes[0..count];
        let dest = &mut buf[0..count];

        dest.copy_from_slice(src);
        self.position += count;
        Ok(count)
    }
}

/// A mock file system driver which stores files in-memory.
///
/// This driver is intended for tests only, and should not be used in production.
pub struct MockFileSystemDriver {
    files: HashMap<AbsolutePath, Rc<Vec<u8>>>,
}

impl MockFileSystemDriver {
    pub fn new() -> Self {
        Self { files: HashMap::new() }
    }

    pub fn add_file(&mut self, path: AbsolutePath, bytes: &[u8]) {
        self.files.insert(path, Rc::new(bytes.to_owned()));
    }

    pub fn add_text_file(&mut self, path: AbsolutePath, text: &str) {
        self.add_file(path, text.as_bytes());
    }
}

impl FileSystemDriver for MockFileSystemDriver {
    fn metadata(&self, path: &AbsolutePath) -> io::Result<Metadata> {
        match self.files.get(path) {
            Some(bytes) => Ok(Metadata::File {
                modified: Ok(SystemTime::UNIX_EPOCH),
                accessed: Ok(SystemTime::UNIX_EPOCH),
                created: Ok(SystemTime::UNIX_EPOCH),
                len: bytes.len() as u64,
                is_readonly: true,
                is_symlink: false,
            }),
            None => Err(io::ErrorKind::NotFound.into()),
        }
    }

    fn create(&self, _path: &AbsolutePath) -> io::Result<Box<dyn Write>> {
        // TODO: write support
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "MockFileSystemDriver does not support creating files",
        ))
    }

    fn create_new(&self, _path: &AbsolutePath) -> io::Result<Box<dyn ReadWrite>> {
        // TODO: write support
        Err(io::Error::new(
            io::ErrorKind::Unsupported,
            "MockFileSystemDriver does not support creating files",
        ))
    }

    fn open(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>> {
        match self.files.get(path).cloned() {
            Some(bytes) => Ok(Box::new(MockFile::new(bytes))),
            None => Err(io::ErrorKind::NotFound.into()),
        }
    }

    fn open_execute(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>> {
        self.open(path)
    }
}

impl Default for MockFileSystemDriver {
    fn default() -> Self {
        Self::new()
    }
}
