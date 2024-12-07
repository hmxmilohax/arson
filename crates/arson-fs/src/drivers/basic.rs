// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    fs::File,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use crate::{AbsolutePath, FileSystemDriver, Metadata, ReadWrite};

/// An extremely basic file system driver which simply provides raw access to a specific directory.
///
/// Because [`AbsolutePath`] and [`FileSystem`](super::FileSystem) handle path sandboxing inherently,
/// this driver prevents sandbox escapes naturally. To doubly ensure this, a paranoid safety assert
/// is done when resolving the virtual path to the on-disk one to ensure no parent directory components
/// slip through.
pub struct BasicFileSystemDriver {
    mount_dir: PathBuf,
}

impl BasicFileSystemDriver {
    /// Creates a new [`BasicFileSystemDriver`] with the given mount directory.
    ///
    /// # Errors
    ///
    /// Returns an error if the given path could not be canonicalized or is not a directory.
    pub fn new(mount_dir: &Path) -> io::Result<Self> {
        let mount_dir = mount_dir.canonicalize()?;
        if !mount_dir.is_dir() {
            return Err(io::ErrorKind::NotADirectory.into());
        }

        Ok(Self { mount_dir })
    }

    fn resolve_path(&self, path: &AbsolutePath) -> PathBuf {
        path.to_fs_path(&self.mount_dir)
    }
}

impl FileSystemDriver for BasicFileSystemDriver {
    fn metadata(&self, path: &AbsolutePath) -> io::Result<Metadata> {
        self.resolve_path(path).metadata().map(Metadata::from)
    }

    fn create(&self, path: &AbsolutePath) -> io::Result<Box<dyn Write>> {
        File::create(self.resolve_path(path)).map::<Box<dyn Write>, _>(|f| Box::new(f))
    }

    fn create_new(&self, path: &AbsolutePath) -> io::Result<Box<dyn ReadWrite>> {
        File::create_new(self.resolve_path(path)).map::<Box<dyn ReadWrite>, _>(|f| Box::new(f))
    }

    fn open(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>> {
        File::open(self.resolve_path(path)).map::<Box<dyn Read>, _>(|f| Box::new(f))
    }

    fn open_execute(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>> {
        self.open(path)
    }
}
