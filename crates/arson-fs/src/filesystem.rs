// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read, Write};

use super::{AbsolutePath, FileSystemDriver, Metadata, ReadWrite, VirtualPath};

/// A file system implementation to be used from scripts.
///
/// All methods which take a path accept relative paths,
/// and resolve them relative to the [current working directory](FileSystem::cwd)
/// of the file system.
pub struct FileSystem {
    driver: Box<dyn FileSystemDriver>,
    cwd: AbsolutePath,
}

impl FileSystem {
    /// Creates a new [`FileSystem`] with the given driver.
    pub fn new<T: FileSystemDriver + 'static>(driver: T) -> Self {
        Self { driver: Box::new(driver), cwd: AbsolutePath::new() }
    }

    /// Gets the current working directory, used to resolve relative paths.
    pub fn cwd(&self) -> &AbsolutePath {
        &self.cwd
    }

    /// Sets a new working directory and returns the old one.
    ///
    /// Relative paths are resolved relative to the existing working directory,
    /// and this resolved path is what will become the new working directory.
    ///
    /// # Errors
    ///
    /// Returns an error if the given path does not exist as a directory.
    pub fn set_cwd<P: AsRef<VirtualPath>>(&mut self, path: P) -> AbsolutePath {
        let mut path = self.canonicalize(path);
        std::mem::swap(&mut self.cwd, &mut path);
        path
    }

    /// Constructs the absolute form of a given path, resolving relative to the
    /// [current working directory](FileSystem::cwd).
    ///
    /// See [`VirtualPath::canonicalize`] for full details.
    pub fn canonicalize<P: AsRef<VirtualPath>>(&self, path: P) -> AbsolutePath {
        path.as_ref().make_absolute(&self.cwd)
    }

    /// Retrieves metadata for the given path, if it exists.
    pub fn metadata<P: AsRef<VirtualPath>>(&self, path: P) -> io::Result<Metadata> {
        self.driver.metadata(&self.canonicalize(path))
    }

    /// Determines whether the given path exists in the file system.
    pub fn exists<P: AsRef<VirtualPath>>(&self, path: P) -> bool {
        self.metadata(path).is_ok()
    }

    /// Determines whether the given path exists and refers to a file.
    pub fn is_file<P: AsRef<VirtualPath>>(&self, path: P) -> bool {
        self.metadata(path).map_or(false, |m| m.is_file())
    }

    /// Determines whether the given path exists and refers to a directory.
    pub fn is_dir<P: AsRef<VirtualPath>>(&self, path: P) -> bool {
        self.metadata(path).map_or(false, |m| m.is_dir())
    }

    /// Opens a file in write-only mode, creating if it doesn't exist yet, and truncating if it does.
    pub fn create<P: AsRef<VirtualPath>>(&self, path: P) -> io::Result<Box<dyn Write>> {
        self.driver.create(&self.canonicalize(path))
    }

    /// Creates a new file in read-write mode. Errors if the file already exists.
    pub fn create_new<P: AsRef<VirtualPath>>(&self, path: P) -> io::Result<Box<dyn ReadWrite>> {
        self.driver.create_new(&self.canonicalize(path))
    }

    /// Opens a file with read permissions.
    pub fn open<P: AsRef<VirtualPath>>(&self, path: P) -> io::Result<Box<dyn Read>> {
        self.driver.open(&self.canonicalize(path))
    }

    /// Opens a file with execute permissions.
    pub fn open_execute<P: AsRef<VirtualPath>>(&self, path: P) -> io::Result<Box<dyn Read>> {
        self.driver.open_execute(&self.canonicalize(path))
    }
}
