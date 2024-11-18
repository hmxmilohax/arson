// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    fs::File,
    io::{self, Read, Write},
    path::{Path, PathBuf},
};

use super::AbsolutePath;

/// Conjunction of the [`Read`] and [`Write`] traits.
///
/// This trait exists to work around `dyn Read + Write` not being allowed,
/// and has no unique functionality. It is auto-implemented for all types
/// which implement both traits.
pub trait ReadWrite: Read + Write {}
impl<T: Read + Write> ReadWrite for T {}

/// A file system driver used to back [`FileSystem`](super::FileSystem).
pub trait FileSystemDriver {
    /// Determines whether the given path exists in the file system.
    fn exists(&self, path: &AbsolutePath) -> bool {
        self.try_exists(path).unwrap_or(false)
    }

    /// Determines whether the given path exists and refers to a file.
    fn is_file(&self, path: &AbsolutePath) -> bool {
        self.try_is_file(path).unwrap_or(false)
    }

    /// Determines whether the given path exists and refers to a directory.
    fn is_dir(&self, path: &AbsolutePath) -> bool {
        self.try_is_dir(path).unwrap_or(false)
    }

    /// Determines whether the given path exists in the file system,
    /// propogating any errors that occur during the process.
    fn try_exists(&self, path: &AbsolutePath) -> io::Result<bool>;

    /// Determines whether the given path exists and refers to a file,
    /// propogating any errors that occur during the process.
    fn try_is_file(&self, path: &AbsolutePath) -> io::Result<bool>;

    /// Determines whether the given path exists and refers to a directory,
    /// propogating any errors that occur during the process.
    fn try_is_dir(&self, path: &AbsolutePath) -> io::Result<bool>;

    /// Opens a file in write-only mode, creating if it doesn't exist yet, and truncating if it does.
    fn create(&self, path: &AbsolutePath) -> io::Result<Box<dyn Write>>;

    /// Creates a new file in read-write mode. Errors if the file already exists.
    fn create_new(&self, path: &AbsolutePath) -> io::Result<Box<dyn ReadWrite>>;

    /// Opens a file with read permissions.
    fn open(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>>;

    /// Opens a file with execute permissions.
    ///
    /// This function is used when opening a file which is to be executed as a script.
    /// The file otherwise has standard read permissions, and this function exists
    /// primarily to allow for additional filtering where desired.
    fn open_execute(&self, path: &AbsolutePath) -> io::Result<Box<dyn Read>>;
}

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
            // FIXME: Use ErrorKind::NotADirectory when that stabilizes
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "not a directory"));
        }

        Ok(Self { mount_dir })
    }

    fn resolve_path(&self, path: &AbsolutePath) -> PathBuf {
        path.to_fs_path(&self.mount_dir)
    }
}

impl FileSystemDriver for BasicFileSystemDriver {
    fn exists(&self, path: &AbsolutePath) -> bool {
        self.resolve_path(path).exists()
    }

    fn is_file(&self, path: &AbsolutePath) -> bool {
        self.resolve_path(path).is_file()
    }

    fn is_dir(&self, path: &AbsolutePath) -> bool {
        self.resolve_path(path).is_dir()
    }

    fn try_exists(&self, path: &AbsolutePath) -> io::Result<bool> {
        self.resolve_path(path).try_exists()
    }

    fn try_is_file(&self, path: &AbsolutePath) -> io::Result<bool> {
        self.resolve_path(path).metadata().map(|m| m.is_file())
    }

    fn try_is_dir(&self, path: &AbsolutePath) -> io::Result<bool> {
        self.resolve_path(path).metadata().map(|m| m.is_dir())
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
