// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read, Write};

use super::{CanonicalizeError, VirtualPath, VirtualPathBuf};

/// Conjunction of the [`Read`] and [`Write`] traits.
///
/// This trait exists to work around `dyn Read + Write` not being allowed,
/// and has no unique functionality. It is auto-implemented for all types
/// which implement both traits.
pub trait ReadWrite: Read + Write {}
impl<T: Read + Write> ReadWrite for T {}

/// A file system implementation to be used from scripts.
///
/// All methods which take a path accept relative paths,
/// and resolve them relative to the [current working directory](FileSystem::cwd)
/// of the file system.
pub trait FileSystem {
    /// Gets the current working directory, used to resolve relative paths.
    fn cwd(&self) -> &VirtualPath;

    /// Sets the current working directory.
    ///
    /// Relative paths are resolved relative to the existing working directory,
    /// and this resolved path is what will become the new working directory.
    ///
    /// # Errors
    ///
    /// Returns an error if the given path does not exist as a directory.
    fn set_cwd(&mut self, path: &VirtualPath) -> io::Result<()>;

    /// Determines whether the given path exists in the file system.
    fn exists(&self, path: &VirtualPath) -> bool;

    /// Determines whether the given path exists and refers to a file.
    fn is_file(&self, path: &VirtualPath) -> bool;

    /// Determines whether the given path exists and refers to a directory.
    fn is_dir(&self, path: &VirtualPath) -> bool;

    /// Opens a file in write-only mode, creating if it doesn't exist yet, and truncating if it does.
    fn create(&self, path: &VirtualPath) -> io::Result<Box<dyn Write>>;

    /// Creates a new file in read-write mode. Errors if the file already exists.
    fn create_new(&self, path: &VirtualPath) -> io::Result<Box<dyn ReadWrite>>;

    /// Opens a file with read permissions.
    fn open(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>>;

    /// Opens a file with execute permissions.
    ///
    /// This function is used when opening a file which is to be executed as a script.
    /// The file otherwise has standard read permissions, and this function exists
    /// primarily to allow for additional filtering where desired.
    fn open_execute(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>>;
}

impl dyn FileSystem {
    /// Constructs the absolute form of a given path, using the
    /// [current working directory](FileSystem::cwd) of the file system.
    ///
    /// See [`VirtualPath::canonicalize`] for full details.
    pub fn canonicalize_path(&self, path: &VirtualPath) -> Result<VirtualPathBuf, CanonicalizeError> {
        path.canonicalize(self.cwd())
    }
}
