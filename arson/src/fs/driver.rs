// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    io::{self, Read, Write},
    path::PathBuf,
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
