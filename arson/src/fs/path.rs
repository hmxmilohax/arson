// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    borrow::Borrow,
    collections::TryReserveError,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VirtualComponent<'path> {
    RootDir,
    CurDir,
    ParentDir,
    Normal(&'path str),
}

#[derive(thiserror::Error, Debug)]
#[error("prefix not found")]
pub struct StripPrefixError(());

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct VirtualPath {
    inner: str,
}

impl VirtualPath {
    pub const SEPARATOR: char = '/';
    pub const SEPARATOR_STR: &str = "/";

    pub fn new<S: AsRef<str> + ?Sized>(s: &S) -> &VirtualPath {
        // SAFETY: VirtualPath transparently contains a str, so its layout is identical
        unsafe { &*(s.as_ref() as *const str as *const VirtualPath) }
    }

    fn from_inner_mut(s: &mut str) -> &mut VirtualPath {
        // SAFETY: VirtualPath transparently contains a str, so its layout is identical
        unsafe { &mut *(s as *mut str as *mut VirtualPath) }
    }

    pub fn as_str(&self) -> &str {
        &self.inner
    }

    pub fn as_mut_str(&mut self) -> &mut str {
        &mut self.inner
    }

    pub fn is_absolute(&self) -> bool {
        self.has_root()
    }

    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }

    pub fn has_root(&self) -> bool {
        self.inner.starts_with('/')
    }

    pub fn components(&self) -> impl Iterator<Item = VirtualComponent<'_>> {
        // Special handling required for the starting component
        let start = self.inner.split_inclusive('/').take(1).map(|s| match s {
            // Root/current directory are only allowed as the starting component
            "/" => VirtualComponent::RootDir,
            "./" => VirtualComponent::CurDir,
            "../" => VirtualComponent::ParentDir,
            component => VirtualComponent::Normal(&component[..component.len() - 1]),
        });

        let splits = self.inner.split('/').skip(1).filter_map(|s| match s {
            // Filter out repeated separators and non-starting '.'
            "" | "." => None,
            ".." => Some(VirtualComponent::ParentDir),
            component => Some(VirtualComponent::Normal(component)),
        });

        start.chain(splits)
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        let start = self.inner.split_inclusive('/').take(1).map(|s| match s {
            "/" => "/",
            component => &component[..component.len() - 1],
        });

        let splits = self.inner.split('/').skip(1).filter_map(|s| match s {
            "" | "." => None,
            component => Some(component),
        });

        start.chain(splits)
    }

    pub fn ancestors(&self) -> impl Iterator<Item = &VirtualPath> {
        std::iter::successors(Some(self), |p| p.parent())
    }

    fn parent_file_name(&self) -> Option<(&str, &str)> {
        self.inner.trim_end_matches('/').rsplit_once('/')
    }

    pub fn parent(&self) -> Option<&VirtualPath> {
        self.parent_file_name().map(|s| VirtualPath::new(s.0))
    }

    pub fn file_name(&self) -> Option<&str> {
        self.parent_file_name().map(|s| s.1)
    }

    fn file_stem_extension(&self) -> Option<(Option<&str>, Option<&str>)> {
        self.file_name().map(|name| match name.rsplit_once('.') {
            Some((stem, extension)) => match stem.is_empty() {
                // Leading '.' and no extension, e.g. ".gitignore"
                true => (Some(extension), None),
                // Typical stem + extension
                false => (Some(stem), Some(extension)),
            },
            // No extension, e.g. "README"
            None => (Some(name), None),
        })
    }

    pub fn file_stem(&self) -> Option<&str> {
        self.file_stem_extension().and_then(|(stem, _)| stem)
    }

    // Unstable feature on Path, not to be implemented until that stabilizes
    // pub fn file_prefix(&self) -> Option<&str>;

    pub fn extension(&self) -> Option<&str> {
        self.file_stem_extension().and_then(|(_, ext)| ext)
    }

    pub fn starts_with<P: AsRef<VirtualPath>>(&self, _base: P) -> bool {
        todo!("VirtualPath::starts_with")
    }

    pub fn ends_with<P: AsRef<VirtualPath>>(&self, _child: P) -> bool {
        todo!("VirtualPath::ends_with")
    }

    pub fn strip_prefix<P: AsRef<VirtualPath>>(&self, _base: P) -> Result<&VirtualPath, StripPrefixError> {
        todo!("VirtualPath::strip_prefix")
    }

    pub fn with_file_name<S: AsRef<str>>(&self, _file_name: S) -> VirtualPathBuf {
        todo!("VirtualPath::with_file_name")
    }

    pub fn with_extension<S: AsRef<str>>(&self, _extension: S) -> VirtualPathBuf {
        todo!("VirtualPath::with_extension")
    }

    // Unstable feature on Path, not to be implemented until that stabilizes
    // pub fn with_added_extension<S: AsRef<str>>(&self, extension: S) -> VirtualPathBuf;

    pub fn join<P: AsRef<VirtualPath>>(&self, path: P) -> VirtualPathBuf {
        let mut buf = path.as_ref().to_buf();
        buf.push(self);
        buf
    }

    // available via VirtualFileSystem
    // pub fn canonicalize(&self) -> VirtualPathBuf;

    pub fn to_buf(&self) -> VirtualPathBuf {
        VirtualPathBuf::from(self)
    }
}

impl Debug for VirtualPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Display for VirtualPath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner)
    }
}

impl AsRef<VirtualPath> for VirtualPath {
    fn as_ref(&self) -> &VirtualPath {
        self
    }
}

impl AsRef<VirtualPath> for str {
    fn as_ref(&self) -> &VirtualPath {
        VirtualPath::new(self)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VirtualPathBuf {
    inner: String,
}

impl VirtualPathBuf {
    pub fn new() -> Self {
        Self { inner: String::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { inner: String::with_capacity(capacity) }
    }

    // Unstable feature on PathBuf, not to be implemented until that stabilizes
    // pub fn leak<'a>(self) -> &'a mut VirtualPath;

    pub fn as_path(&self) -> &VirtualPath {
        VirtualPath::new(&self.inner)
    }

    pub fn as_string(&self) -> &String {
        &self.inner
    }

    pub fn as_mut_string(&mut self) -> &mut String {
        &mut self.inner
    }

    pub fn push<P: AsRef<VirtualPath>>(&mut self, path: P) {
        let path = path.as_ref();

        if path.is_absolute() {
            self.inner.clear();
        } else {
            // `path` being absolute means it already starts with a '/',
            // and so we only need to check our own buffer for a separator
            if !self.inner.ends_with('/') {
                self.inner.push('/');
            }
        }

        self.inner.push_str(&path.inner);
    }

    pub fn pop(&mut self) -> bool {
        match self.parent() {
            Some(parent) => {
                self.inner.truncate(parent.as_str().len());
                true
            },
            None => false,
        }
    }

    pub fn set_file_name<S: AsRef<str>>(&mut self, _file_name: S) {
        todo!("VirtualPathBuf::set_file_name")
    }

    pub fn set_extension<S: AsRef<str>>(&mut self, _extension: S) -> bool {
        todo!("VirtualPathBuf::set_extension")
    }

    // Unstable feature on PathBuf, not to be implemented until that stabilizes
    // pub fn add_extension<S: AsRef<str>>(&mut self, extension: S) -> bool;

    pub fn capacity(&self) -> usize {
        self.inner.capacity()
    }

    pub fn clear(&mut self) {
        self.inner.clear()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.inner.reserve(additional)
    }

    pub fn try_reserve(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve(additional)
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.inner.reserve_exact(additional)
    }

    pub fn try_reserve_exact(&mut self, additional: usize) -> Result<(), TryReserveError> {
        self.inner.try_reserve_exact(additional)
    }

    pub fn shrink_to_fit(&mut self) {
        self.inner.shrink_to_fit()
    }

    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.inner.shrink_to(min_capacity)
    }
}

impl Debug for VirtualPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Display for VirtualPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.inner)
    }
}

impl Default for VirtualPathBuf {
    fn default() -> Self {
        Self::new()
    }
}

impl Deref for VirtualPathBuf {
    type Target = VirtualPath;

    fn deref(&self) -> &Self::Target {
        VirtualPath::new(&self.inner)
    }
}

impl DerefMut for VirtualPathBuf {
    fn deref_mut(&mut self) -> &mut Self::Target {
        VirtualPath::from_inner_mut(&mut self.inner)
    }
}

impl<T> AsRef<T> for VirtualPathBuf
where
    T: ?Sized,
    <VirtualPathBuf as Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        self.deref().as_ref()
    }
}

impl Borrow<VirtualPath> for VirtualPathBuf {
    fn borrow(&self) -> &VirtualPath {
        self.deref()
    }
}

impl From<&str> for VirtualPathBuf {
    fn from(value: &str) -> Self {
        Self { inner: value.to_owned() }
    }
}

impl From<&String> for VirtualPathBuf {
    fn from(value: &String) -> Self {
        Self { inner: value.clone() }
    }
}

impl From<String> for VirtualPathBuf {
    fn from(value: String) -> Self {
        Self { inner: value }
    }
}

impl From<&VirtualPath> for VirtualPathBuf {
    fn from(value: &VirtualPath) -> Self {
        Self { inner: value.inner.to_owned() }
    }
}
