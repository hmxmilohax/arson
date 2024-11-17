// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read, Write};

use super::{AbsolutePath, AbsolutePathBuf, FileSystemDriver, ReadWrite, VirtualPath};

/// A file system implementation to be used from scripts.
///
/// All methods which take a path accept relative paths,
/// and resolve them relative to the [current working directory](FileSystem::cwd)
/// of the file system.
pub struct FileSystem {
    driver: Box<dyn FileSystemDriver>,
    cwd: AbsolutePathBuf,
}

impl FileSystem {
    /// Creates a new [`FileSystem`] with the given driver.
    pub fn new(driver: Box<dyn FileSystemDriver>) -> Self {
        Self { driver, cwd: AbsolutePathBuf::new() }
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
    pub fn set_cwd(&mut self, path: &VirtualPath) -> AbsolutePathBuf {
        let mut path = self.canonicalize(path);
        std::mem::swap(&mut self.cwd, &mut path);
        path
    }

    /// Constructs the absolute form of a given path, resolving relative to the
    /// [current working directory](FileSystem::cwd).
    ///
    /// See [`VirtualPath::canonicalize`] for full details.
    pub fn canonicalize(&self, path: &VirtualPath) -> AbsolutePathBuf {
        path.make_absolute(&self.cwd)
    }

    /// Determines whether the given path exists in the file system.
    pub fn exists(&self, path: &VirtualPath) -> bool {
        let path = self.canonicalize(path);
        self.driver.exists(&path)
    }

    /// Determines whether the given path exists and refers to a file.
    pub fn is_file(&self, path: &VirtualPath) -> bool {
        let path = self.canonicalize(path);
        self.driver.is_file(&path)
    }

    /// Determines whether the given path exists and refers to a directory.
    pub fn is_dir(&self, path: &VirtualPath) -> bool {
        let path = self.canonicalize(path);
        self.driver.is_dir(&path)
    }

    /// Opens a file in write-only mode, creating if it doesn't exist yet, and truncating if it does.
    pub fn create(&self, path: &VirtualPath) -> io::Result<Box<dyn Write>> {
        let path = self.canonicalize(path);
        self.driver.create(&path)
    }

    /// Creates a new file in read-write mode. Errors if the file already exists.
    pub fn create_new(&self, path: &VirtualPath) -> io::Result<Box<dyn ReadWrite>> {
        let path = self.canonicalize(path);
        self.driver.create_new(&path)
    }

    /// Opens a file with read permissions.
    pub fn open(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>> {
        let path = self.canonicalize(path);
        self.driver.open(&path)
    }

    /// Opens a file with execute permissions.
    pub fn open_execute(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>> {
        let path = self.canonicalize(path);
        self.driver.open_execute(&path)
    }
}
