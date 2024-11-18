// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    fs::File,
    io::{self, Read, Write},
    path::{Path, PathBuf},
    time::SystemTime,
};

use super::AbsolutePath;

/// Conjunction of the [`Read`] and [`Write`] traits.
///
/// This trait exists to work around `dyn Read + Write` not being allowed,
/// and has no unique functionality. It is auto-implemented for all types
/// which implement both traits.
pub trait ReadWrite: Read + Write {}
impl<T: Read + Write> ReadWrite for T {}

/// Metadata information for a file system entry.
pub enum Metadata {
    File {
        modified: io::Result<SystemTime>,
        accessed: io::Result<SystemTime>,
        created: io::Result<SystemTime>,

        len: u64,

        is_readonly: bool,
        is_symlink: bool,
    },
    Directory {
        modified: io::Result<SystemTime>,
        accessed: io::Result<SystemTime>,
        created: io::Result<SystemTime>,

        is_symlink: bool,
    },
}

/// A file system driver used to back [`FileSystem`](super::FileSystem).
pub trait FileSystemDriver {
    /// Retrieves metadata for the entry referred to by the given path.
    fn metadata(&self, path: &AbsolutePath) -> io::Result<Metadata>;

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

impl Metadata {
    pub fn is_file(&self) -> bool {
        matches!(self, Self::File { .. })
    }

    pub fn is_dir(&self) -> bool {
        matches!(self, Self::Directory { .. })
    }

    pub fn is_symlink(&self) -> bool {
        match self {
            Self::File { is_symlink, .. } => *is_symlink,
            Self::Directory { is_symlink, .. } => *is_symlink,
        }
    }

    pub fn modified(&self) -> &io::Result<SystemTime> {
        match self {
            Self::File { modified, .. } => modified,
            Self::Directory { modified, .. } => modified,
        }
    }

    pub fn accessed(&self) -> &io::Result<SystemTime> {
        match self {
            Self::File { accessed, .. } => accessed,
            Self::Directory { accessed, .. } => accessed,
        }
    }

    pub fn created(&self) -> &io::Result<SystemTime> {
        match self {
            Self::File { created, .. } => created,
            Self::Directory { created, .. } => created,
        }
    }
}

impl From<std::fs::Metadata> for Metadata {
    fn from(value: std::fs::Metadata) -> Self {
        Self::from(&value)
    }
}

impl From<&std::fs::Metadata> for Metadata {
    fn from(value: &std::fs::Metadata) -> Self {
        if value.is_file() {
            Self::File {
                len: value.len(),

                is_readonly: value.permissions().readonly(),
                is_symlink: value.is_symlink(),

                modified: value.modified(),
                accessed: value.accessed(),
                created: value.created(),
            }
        } else {
            Self::Directory {
                is_symlink: value.is_symlink(),

                modified: value.modified(),
                accessed: value.accessed(),
                created: value.created(),
            }
        }
    }
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
    fn metadata(&self, path: &AbsolutePath) -> io::Result<Metadata> {
        self.resolve_path(path)
            .metadata()
            .map(|m| Metadata::from(m))
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
