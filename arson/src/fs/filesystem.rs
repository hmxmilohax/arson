// SPDX-License-Identifier: LGPL-3.0-or-later

use std::io::{self, Read, Write};

use super::{AbsolutePath, FileSystemDriver, ReadWrite, VirtualPath};

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
    pub fn new(driver: Box<dyn FileSystemDriver>) -> Self {
        Self { driver, cwd: AbsolutePath::new() }
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
    pub fn set_cwd(&mut self, path: &VirtualPath) -> AbsolutePath {
        let mut path = self.canonicalize(path);
        std::mem::swap(&mut self.cwd, &mut path);
        path
    }

    /// Constructs the absolute form of a given path, resolving relative to the
    /// [current working directory](FileSystem::cwd).
    ///
    /// See [`VirtualPath::canonicalize`] for full details.
    pub fn canonicalize(&self, path: &VirtualPath) -> AbsolutePath {
        path.make_absolute(&self.cwd)
    }

    /// Determines whether the given path exists in the file system.
    pub fn exists(&self, path: &VirtualPath) -> bool {
        self.driver.exists(&self.canonicalize(path))
    }

    /// Determines whether the given path exists and refers to a file.
    pub fn is_file(&self, path: &VirtualPath) -> bool {
        self.driver.is_file(&self.canonicalize(path))
    }

    /// Determines whether the given path exists and refers to a directory.
    pub fn is_dir(&self, path: &VirtualPath) -> bool {
        self.driver.is_dir(&self.canonicalize(path))
    }

    /// Determines whether the given path exists in the file system,
    /// propogating any errors that occur during the process.
    pub fn try_exists(&self, path: &VirtualPath) -> io::Result<bool> {
        self.driver.try_exists(&self.canonicalize(path))
    }

    /// Determines whether the given path exists and refers to a file,
    /// propogating any errors that occur during the process.
    pub fn try_is_file(&self, path: &VirtualPath) -> io::Result<bool> {
        self.driver.try_is_file(&self.canonicalize(path))
    }

    /// Determines whether the given path exists and refers to a directory,
    /// propogating any errors that occur during the process.
    pub fn try_is_dir(&self, path: &VirtualPath) -> io::Result<bool> {
        self.driver.try_is_dir(&self.canonicalize(path))
    }

    /// Opens a file in write-only mode, creating if it doesn't exist yet, and truncating if it does.
    pub fn create(&self, path: &VirtualPath) -> io::Result<Box<dyn Write>> {
        self.driver.create(&self.canonicalize(path))
    }

    /// Creates a new file in read-write mode. Errors if the file already exists.
    pub fn create_new(&self, path: &VirtualPath) -> io::Result<Box<dyn ReadWrite>> {
        self.driver.create_new(&self.canonicalize(path))
    }

    /// Opens a file with read permissions.
    pub fn open(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>> {
        self.driver.open(&self.canonicalize(path))
    }

    /// Opens a file with execute permissions.
    pub fn open_execute(&self, path: &VirtualPath) -> io::Result<Box<dyn Read>> {
        self.driver.open_execute(&self.canonicalize(path))
    }
}
