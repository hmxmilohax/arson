// SPDX-License-Identifier: LGPL-3.0-or-later

use std::{
    borrow::Borrow,
    collections::TryReserveError,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
};

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
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

    pub fn components(&self) -> VirtualComponents<'_> {
        VirtualComponents::new(self)
    }

    pub fn iter(&self) -> VirtualPathIter<'_> {
        VirtualPathIter { inner: VirtualComponents::new(self) }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = &VirtualPath> {
        std::iter::successors(Some(self), |p| p.parent())
    }

    fn parent_file_name(&self) -> Option<(&str, &str)> {
        // Trim all trailing remaining separators
        let trimmed = self.inner.trim_end_matches('/');

        match trimmed.rsplit_once('/') {
            Some((mut parent, file_name)) => {
                // Trim any spurious remaining separators
                parent = parent.trim_end_matches('/');

                // Ensure root directory is preserved as the last parent
                if parent.is_empty() && trimmed.starts_with('/') {
                    parent = "/";
                }

                Some((parent, file_name))
            },
            None => None,
        }
    }

    pub fn parent(&self) -> Option<&VirtualPath> {
        self.parent_file_name().map(|s| VirtualPath::new(s.0))
    }

    pub fn file_name(&self) -> Option<&str> {
        self.parent_file_name().map(|s| s.1)
    }

    fn file_stem_extension(&self) -> Option<(&str, Option<&str>)> {
        self.file_name().map(|name| match name.rsplit_once('.') {
            Some((stem, extension)) => match stem.is_empty() {
                // Leading '.' and no extension, e.g. ".gitignore"
                true => (name, None),
                // Typical stem + extension
                false => (stem, Some(extension)),
            },
            // No extension, e.g. "README"
            None => (name, None),
        })
    }

    pub fn file_stem(&self) -> Option<&str> {
        self.file_stem_extension().map(|(stem, _)| stem)
    }

    // Unstable feature on Path, not to be implemented until that stabilizes
    // pub fn file_prefix(&self) -> Option<&str>;

    pub fn extension(&self) -> Option<&str> {
        self.file_stem_extension().and_then(|(_, ext)| ext)
    }

    fn match_prefix<'a, 'b, A, B>(mut iter: A, prefix: B) -> Option<A>
    where
        A: Iterator<Item = VirtualComponent<'a>>,
        B: Iterator<Item = VirtualComponent<'b>>,
    {
        for component in prefix {
            if iter.next() != Some(component) {
                return None;
            }
        }

        Some(iter)
    }

    pub fn starts_with<P: AsRef<VirtualPath>>(&self, base: P) -> bool {
        Self::match_prefix(self.components(), base.as_ref().components()).is_some()
    }

    pub fn ends_with<P: AsRef<VirtualPath>>(&self, child: P) -> bool {
        Self::match_prefix(self.components().rev(), child.as_ref().components().rev()).is_some()
    }

    pub fn strip_prefix<P: AsRef<VirtualPath>>(&self, base: P) -> Result<&VirtualPath, StripPrefixError> {
        Self::match_prefix(self.components(), base.as_ref().components())
            .map(|c| c.as_path())
            .ok_or(StripPrefixError(()))
    }

    pub fn with_file_name<S: AsRef<str>>(&self, file_name: S) -> VirtualPathBuf {
        let mut buf = self.to_buf();
        buf.set_file_name(file_name);
        buf
    }

    pub fn with_extension<S: AsRef<str>>(&self, extension: S) -> VirtualPathBuf {
        let mut buf = self.to_buf();
        buf.set_extension(extension);
        buf
    }

    // Unstable feature on Path, not to be implemented until that stabilizes
    // pub fn with_added_extension<S: AsRef<str>>(&self, extension: S) -> VirtualPathBuf;

    pub fn join<P: AsRef<VirtualPath>>(&self, path: P) -> VirtualPathBuf {
        let mut buf = self.to_buf();
        buf.push(path);
        buf
    }

    /// Constructs the absolute form of this path using the provided base path.
    ///
    /// Relative paths are resolved according to the base path.
    /// Then, all current directory (`.`) and parent directory (`..`) components are resolved,
    /// ignoring any attempts to go beyond the root path.
    pub fn make_absolute(&self, base: &AbsolutePath) -> AbsolutePath {
        let path = base.join(self);

        // Sanity check to ensure path starts with the root at this point
        assert_eq!(path.components().next(), Some(VirtualComponent::RootDir));

        // Resolve all path components
        let mut path_stack = vec![];
        for component in path.components() {
            match component {
                VirtualComponent::RootDir => continue,
                VirtualComponent::CurDir => continue,
                VirtualComponent::ParentDir => {
                    path_stack.pop();
                },
                VirtualComponent::Normal(name) => path_stack.push(name),
            };
        }

        // Collect everything into the final path, starting with the root prefix
        let mut path = AbsolutePath::new();
        path.inner.push(path_stack.join("/"));
        assert!(path.inner.is_absolute());
        path
    }

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

impl AsRef<VirtualPath> for String {
    fn as_ref(&self) -> &VirtualPath {
        VirtualPath::new(self)
    }
}

impl AsRef<VirtualPath> for str {
    fn as_ref(&self) -> &VirtualPath {
        VirtualPath::new(self)
    }
}

impl ToOwned for VirtualPath {
    type Owned = VirtualPathBuf;

    fn to_owned(&self) -> Self::Owned {
        VirtualPathBuf::from(&self.inner)
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

    pub fn set_file_name<S: AsRef<str>>(&mut self, file_name: S) {
        if self.file_name().is_some() {
            self.pop();
        }
        self.push(file_name.as_ref());
    }

    pub fn set_extension<S: AsRef<str>>(&mut self, extension: S) -> bool {
        let extension = extension.as_ref();
        if extension.contains('/') {
            // This would typically be a panic, but allowing scripts to
            // cause a panic through bad input is not the best idea lol
            return false;
        }

        // Perform file name check and grab the extension in one call
        let Some((_, existing)) = self.file_stem_extension() else {
            return false;
        };

        // Remove existing extension
        if let Some(existing) = existing {
            self.inner.truncate(self.inner.len() - (existing.len() + 1));
        }

        // Add new extension
        if !extension.is_empty() {
            self.inner.reserve(extension.len() + 1);
            self.inner.push('.');
            self.inner.push_str(extension);
        }

        true
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

/// A path which is verified to be absolute.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AbsolutePath {
    inner: VirtualPathBuf,
}

impl AbsolutePath {
    pub fn new() -> Self {
        Self { inner: VirtualPathBuf::from("/") }
    }

    pub fn try_new<S: AsRef<VirtualPath> + ?Sized>(s: &S) -> Option<AbsolutePath> {
        let path = s.as_ref();
        if !path.is_absolute() {
            return None;
        }

        Some(Self { inner: path.to_buf() })
    }

    pub fn as_path(&self) -> &VirtualPath {
        self.inner.as_path()
    }

    pub fn as_string(&self) -> &String {
        self.inner.as_string()
    }

    pub fn as_str(&self) -> &str {
        self.inner.as_str()
    }

    pub fn iter(&self) -> AbsolutePathIter<'_> {
        AbsolutePathIter { remaining: self.as_str() }
    }

    pub fn ancestors(&self) -> impl Iterator<Item = &VirtualPath> {
        self.inner.ancestors()
    }

    pub fn parent(&self) -> Option<&VirtualPath> {
        self.inner.parent()
    }

    pub fn file_name(&self) -> Option<&str> {
        self.inner.file_name()
    }

    pub fn file_stem(&self) -> Option<&str> {
        self.inner.file_stem()
    }

    pub fn extension(&self) -> Option<&str> {
        self.inner.extension()
    }

    pub fn starts_with<P: AsRef<VirtualPath>>(&self, base: P) -> bool {
        self.inner.starts_with(base)
    }

    pub fn ends_with<P: AsRef<VirtualPath>>(&self, child: P) -> bool {
        self.inner.ends_with(child)
    }

    pub fn strip_prefix<P: AsRef<VirtualPath>>(&self, base: P) -> Result<&VirtualPath, StripPrefixError> {
        self.inner.strip_prefix(base)
    }

    pub fn with_file_name<S: AsRef<str>>(&self, file_name: S) -> VirtualPathBuf {
        self.inner.with_file_name(file_name)
    }

    pub fn with_extension<S: AsRef<str>>(&self, extension: S) -> VirtualPathBuf {
        self.inner.with_extension(extension)
    }

    pub fn join<P: AsRef<VirtualPath>>(&self, path: P) -> VirtualPathBuf {
        self.inner.join(path)
    }

    pub fn join_absolute<P: AsRef<VirtualPath>>(&self, path: P) -> AbsolutePath {
        path.as_ref().make_absolute(self)
    }

    pub fn to_buf(&self) -> VirtualPathBuf {
        self.inner.clone()
    }
}

impl Debug for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl Display for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.inner, f)
    }
}

impl Default for AbsolutePath {
    fn default() -> Self {
        Self::new()
    }
}

impl AsRef<AbsolutePath> for AbsolutePath {
    fn as_ref(&self) -> &AbsolutePath {
        self
    }
}

impl AsRef<VirtualPath> for AbsolutePath {
    fn as_ref(&self) -> &VirtualPath {
        &self.inner
    }
}

impl Borrow<VirtualPath> for AbsolutePath {
    fn borrow(&self) -> &VirtualPath {
        self.as_ref()
    }
}

impl Borrow<VirtualPathBuf> for AbsolutePath {
    fn borrow(&self) -> &VirtualPathBuf {
        &self.inner
    }
}

#[derive(thiserror::Error, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[error("given path is not absolute")]
pub struct TryFromPathError(());

impl TryFrom<&str> for AbsolutePath {
    type Error = TryFromPathError;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        Self::try_from(VirtualPath::new(value))
    }
}

impl TryFrom<&String> for AbsolutePath {
    type Error = TryFromPathError;

    fn try_from(value: &String) -> Result<Self, Self::Error> {
        Self::try_from(VirtualPath::new(value))
    }
}

impl TryFrom<String> for AbsolutePath {
    type Error = TryFromPathError;

    fn try_from(value: String) -> Result<Self, Self::Error> {
        match AbsolutePath::try_new(&value) {
            Some(_) => Ok(Self { inner: VirtualPathBuf::from(value) }),
            None => Err(TryFromPathError(())),
        }
    }
}

impl TryFrom<&VirtualPath> for AbsolutePath {
    type Error = TryFromPathError;

    fn try_from(value: &VirtualPath) -> Result<Self, Self::Error> {
        match AbsolutePath::try_new(value) {
            Some(path) => Ok(path.to_owned()),
            None => Err(TryFromPathError(())),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum VirtualComponent<'path> {
    RootDir,
    CurDir,
    ParentDir,
    Normal(&'path str),
}

#[derive(Clone, Copy)]
pub struct VirtualComponents<'path> {
    path: &'path str,
    remaining: &'path str,
}

#[derive(Clone, Copy)]
pub struct VirtualPathIter<'path> {
    inner: VirtualComponents<'path>,
}

impl<'path> VirtualComponents<'path> {
    fn new(path: &'path VirtualPath) -> Self {
        Self { path: &path.inner, remaining: &path.inner }
    }

    pub fn as_path(&self) -> &'path VirtualPath {
        VirtualPath::new(self.remaining)
    }

    #[inline]
    fn set_remaining_front(&mut self, remaining: &'path str) {
        self.remaining = remaining.trim_start_matches('/');
    }

    #[inline]
    fn set_remaining_back(&mut self, remaining: &'path str) {
        self.remaining = remaining.trim_end_matches('/');
    }

    #[inline]
    fn bump_remaining_front(&mut self, amount: usize) {
        self.set_remaining_front(&self.remaining[amount..]);
    }

    #[inline]
    fn handle_first_component(&self, text: &'path str) -> Option<(VirtualComponent<'path>, usize)> {
        if std::ptr::addr_eq(text, self.path) {
            if self.path.starts_with('/') {
                return Some((VirtualComponent::RootDir, 1));
            } else if self.path.starts_with("./") {
                return Some((VirtualComponent::CurDir, 2));
            }
        }

        None
    }

    #[inline]
    fn parse_component(text: &'path str) -> Option<VirtualComponent<'path>> {
        match text {
            // Filter out repeated separators and non-starting '.'
            "" => None,
            "." => None,
            ".." => Some(VirtualComponent::ParentDir),
            current => Some(VirtualComponent::Normal(current)),
        }
    }
}

impl<'path> Iterator for VirtualComponents<'path> {
    type Item = VirtualComponent<'path>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some((component, amount_matched)) = self.handle_first_component(self.remaining) {
            self.bump_remaining_front(amount_matched);
            return Some(component);
        }

        loop {
            return match self.remaining.split_once('/') {
                Some((current, remaining)) => {
                    self.set_remaining_front(remaining);
                    match Self::parse_component(current) {
                        Some(c) => Some(c),
                        None => continue,
                    }
                },
                None => {
                    let current = self.remaining;
                    self.remaining = "";
                    Self::parse_component(current)
                },
            };
        }
    }
}

impl<'path> DoubleEndedIterator for VirtualComponents<'path> {
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            return match self.remaining.rsplit_once('/') {
                Some((remaining, current)) => {
                    self.set_remaining_back(remaining);
                    match Self::parse_component(current) {
                        Some(c) => Some(c),
                        None => continue,
                    }
                },
                None => {
                    let last = self.remaining;
                    self.remaining = "";

                    match self.handle_first_component(last) {
                        Some((component, _)) => Some(component),
                        None => Self::parse_component(last),
                    }
                },
            };
        }
    }
}

impl<'path> VirtualPathIter<'path> {
    fn map_component(component: VirtualComponent<'path>) -> &'path str {
        match component {
            VirtualComponent::RootDir => "/",
            VirtualComponent::CurDir => ".",
            VirtualComponent::ParentDir => "..",
            VirtualComponent::Normal(name) => name,
        }
    }
}

impl<'path> Iterator for VirtualPathIter<'path> {
    type Item = &'path str;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(Self::map_component)
    }
}

impl<'path> DoubleEndedIterator for VirtualPathIter<'path> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(Self::map_component)
    }
}

#[derive(Clone, Copy)]
pub struct AbsolutePathIter<'path> {
    remaining: &'path str,
}

impl<'path> Iterator for AbsolutePathIter<'path> {
    type Item = &'path str;

    fn next(&mut self) -> Option<Self::Item> {
        match self.remaining.trim_start_matches('/').split_once('/') {
            Some((component, remaining)) => {
                self.remaining = remaining;
                Some(component)
            },
            None => None,
        }
    }
}

impl<'path> DoubleEndedIterator for AbsolutePathIter<'path> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.remaining.trim_end_matches('/').rsplit_once('/') {
            Some((component, remaining)) => {
                self.remaining = remaining;
                Some(component)
            },
            None => None,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    use VirtualPath as Path;
    use VirtualPathBuf as PathBuf;

    mod path {
        use super::*;

        #[test]
        fn is_absolute() {
            assert!(Path::new("/foo/bar.rs").is_absolute());
            assert!(!Path::new("foo/bar.rs").is_absolute());
        }

        #[test]
        fn is_relative() {
            assert!(!Path::new("/foo/bar.rs").is_relative());
            assert!(Path::new("foo/bar.rs").is_relative());
        }

        #[test]
        fn has_root() {
            assert!(Path::new("/foo/bar.rs").has_root());
            assert!(!Path::new("foo/bar.rs").has_root());
        }

        #[test]
        fn components() {
            // forward
            let mut components = Path::new("/foo/bar.rs").components();
            assert_eq!(components.next(), Some(VirtualComponent::RootDir));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("bar.rs")));
            assert_eq!(components.next(), None);

            let mut components = Path::new("./foo/..//bar/").components();
            assert_eq!(components.next(), Some(VirtualComponent::CurDir));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next(), Some(VirtualComponent::ParentDir));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("bar")));
            assert_eq!(components.next(), None);

            // backward
            let mut components = Path::new("/foo/bar.rs").components();
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("bar.rs")));
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next_back(), Some(VirtualComponent::RootDir));
            assert_eq!(components.next_back(), None);

            let mut components = Path::new("./foo/..//bar/").components();
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("bar")));
            assert_eq!(components.next_back(), Some(VirtualComponent::ParentDir));
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next_back(), Some(VirtualComponent::CurDir));
            assert_eq!(components.next_back(), None);

            // alternating
            let mut components = Path::new("/foo/bar.rs").components();
            assert_eq!(components.next(), Some(VirtualComponent::RootDir));
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("bar.rs")));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next_back(), None);
            assert_eq!(components.next(), None);

            let mut components = Path::new("./foo/..//bar/").components();
            assert_eq!(components.next(), Some(VirtualComponent::CurDir));
            assert_eq!(components.next_back(), Some(VirtualComponent::Normal("bar")));
            assert_eq!(components.next(), Some(VirtualComponent::Normal("foo")));
            assert_eq!(components.next_back(), Some(VirtualComponent::ParentDir));
            assert_eq!(components.next(), None);
            assert_eq!(components.next_back(), None);
        }

        #[test]
        fn iter() {
            // forward
            let mut iter = Path::new("/foo/bar.rs").iter();
            assert_eq!(iter.next(), Some("/"));
            assert_eq!(iter.next(), Some("foo"));
            assert_eq!(iter.next(), Some("bar.rs"));
            assert_eq!(iter.next(), None);

            let mut iter = Path::new("./foo/..//bar/").iter();
            assert_eq!(iter.next(), Some("."));
            assert_eq!(iter.next(), Some("foo"));
            assert_eq!(iter.next(), Some(".."));
            assert_eq!(iter.next(), Some("bar"));
            assert_eq!(iter.next(), None);

            // backward
            let mut iter = Path::new("/foo/bar.rs").iter();
            assert_eq!(iter.next_back(), Some("bar.rs"));
            assert_eq!(iter.next_back(), Some("foo"));
            assert_eq!(iter.next_back(), Some("/"));
            assert_eq!(iter.next_back(), None);

            let mut iter = Path::new("./foo/..//bar/").iter();
            assert_eq!(iter.next_back(), Some("bar"));
            assert_eq!(iter.next_back(), Some(".."));
            assert_eq!(iter.next_back(), Some("foo"));
            assert_eq!(iter.next_back(), Some("."));
            assert_eq!(iter.next_back(), None);

            // alternating
            let mut iter = Path::new("/foo/bar.rs").iter();
            assert_eq!(iter.next(), Some("/"));
            assert_eq!(iter.next_back(), Some("bar.rs"));
            assert_eq!(iter.next(), Some("foo"));
            assert_eq!(iter.next_back(), None);
            assert_eq!(iter.next(), None);

            let mut iter = Path::new("./foo/..//bar/").iter();
            assert_eq!(iter.next(), Some("."));
            assert_eq!(iter.next_back(), Some("bar"));
            assert_eq!(iter.next(), Some("foo"));
            assert_eq!(iter.next_back(), Some(".."));
            assert_eq!(iter.next(), None);
            assert_eq!(iter.next_back(), None);
        }

        #[test]
        fn ancestors() {
            let mut ancestors = Path::new("/foo/bar.rs").ancestors();
            assert_eq!(ancestors.next(), Some(Path::new("/foo/bar.rs")));
            assert_eq!(ancestors.next(), Some(Path::new("/foo")));
            assert_eq!(ancestors.next(), Some(Path::new("/")));
            assert_eq!(ancestors.next(), None);

            let mut ancestors = Path::new("./foo/..//bar/").ancestors();
            assert_eq!(ancestors.next(), Some(Path::new("./foo/..//bar/")));
            assert_eq!(ancestors.next(), Some(Path::new("./foo/..")));
            assert_eq!(ancestors.next(), Some(Path::new("./foo")));
            assert_eq!(ancestors.next(), Some(Path::new(".")));
            assert_eq!(ancestors.next(), None);
        }

        #[test]
        fn parent() {
            assert_eq!(Path::new("/foo/bar.rs").parent(), Some(Path::new("/foo")));
            assert_eq!(Path::new("/foo/").parent(), Some(Path::new("/")));
            assert_eq!(Path::new("/").parent(), None);
        }

        #[test]
        fn file_name() {
            assert_eq!(Path::new("/foo/bar.rs").file_name(), Some("bar.rs"));
            assert_eq!(Path::new("/foo").file_name(), Some("foo"));
            assert_eq!(Path::new("/").file_name(), None);
        }

        #[test]
        fn file_stem() {
            assert_eq!(Path::new("/foo/bar.rs").file_stem(), Some("bar"));
            assert_eq!(Path::new("/foo/bar.tar.gz").file_stem(), Some("bar.tar"));
            assert_eq!(Path::new("/foo/.bar.rs").file_stem(), Some(".bar"));
            assert_eq!(Path::new("/foo/.bar").file_stem(), Some(".bar"));
            assert_eq!(Path::new("/foo").file_stem(), Some("foo"));
            assert_eq!(Path::new("/").file_stem(), None);
        }

        #[test]
        fn extension() {
            assert_eq!(Path::new("/foo/bar.rs").extension(), Some("rs"));
            assert_eq!(Path::new("/foo/bar.tar.gz").extension(), Some("gz"));
            assert_eq!(Path::new("/foo/.bar.rs").extension(), Some("rs"));
            assert_eq!(Path::new("/foo/.bar").extension(), None);
            assert_eq!(Path::new("/foo").extension(), None);
            assert_eq!(Path::new("/").extension(), None);
        }

        #[test]
        fn starts_with() {
            assert!(Path::new("/foo/bar.rs").starts_with("/"));
            assert!(Path::new("/foo/bar.rs").starts_with("/foo"));
            assert!(Path::new("/foo/bar.rs/").starts_with("/foo"));
            assert!(Path::new("//foo/bar.rs").starts_with("/foo"));
            assert!(Path::new("/foo//bar.rs").starts_with("/foo"));
            assert!(Path::new("/foo/bar.rs").starts_with("/foo/"));
            assert!(Path::new("/foo/bar.rs").starts_with("/foo///"));
            assert!(Path::new("./foo/..//bar/").starts_with("."));
            assert!(Path::new("./foo/..//bar/").starts_with("./foo"));

            assert!(!Path::new("/foo/bar.rs").starts_with("/f"));
            assert!(!Path::new("/foo/bar.rs").starts_with("foo"));
            assert!(!Path::new("/foo/bar.rs").starts_with("/foo/bar"));
            assert!(!Path::new("/foo/bar").starts_with("/foo/bar.rs"));
        }

        #[test]
        fn ends_with() {
            assert!(Path::new("/foo/bar.rs").ends_with("bar.rs"));
            assert!(Path::new("/foo/bar.rs/").ends_with("bar.rs"));
            assert!(Path::new("//foo/bar.rs").ends_with("bar.rs"));
            assert!(Path::new("/foo//bar.rs").ends_with("bar.rs"));
            assert!(Path::new("/foo/bar.rs").ends_with("bar.rs/"));
            assert!(Path::new("/foo/bar.rs").ends_with("bar.rs///"));
            assert!(Path::new("./foo/..//bar/").ends_with("bar"));

            assert!(!Path::new("/foo/bar.rs").ends_with("b"));
            assert!(!Path::new("/foo/bar.rs").ends_with("bar"));
            assert!(!Path::new("/foo/bar.rs").ends_with(".rs"));
            assert!(!Path::new("/foo/bar.rs").ends_with("/bar.rs"));
            assert!(!Path::new("/foo/bar.rs").ends_with("/foo/bar"));
            assert!(!Path::new("/foo/bar").ends_with("/foo/bar.rs"));
        }

        #[test]
        fn strip_prefix() {
            fn assert_stripped(start: &str, strip: &str, expected: &str) {
                assert_eq!(Path::new(start).strip_prefix(strip), Ok(Path::new(expected)));
            }

            fn assert_err(start: &str, strip: &str) {
                assert_eq!(Path::new(start).strip_prefix(strip), Err(StripPrefixError(())));
            }

            assert_stripped("/foo/bar.rs", "/foo", "bar.rs");
            assert_stripped("/foo/bar.rs/", "/foo", "bar.rs/");
            assert_stripped("//foo/bar.rs", "/foo", "bar.rs");
            assert_stripped("/foo//bar.rs", "/foo", "bar.rs");
            assert_stripped("/foo/bar.rs", "/foo/", "bar.rs");
            assert_stripped("/foo/bar.rs", "/foo///", "bar.rs");
            assert_stripped("/foo/bar.rs", "/foo/bar.rs", "");

            assert_stripped("./foo/..//bar/", "./foo/..", "bar/");
            assert_stripped("./foo/..//bar/", "./foo/../", "bar/");
            assert_stripped("./foo/..//bar/", "./foo/..//", "bar/");

            assert_err("/foo/bar.rs", "/f");
            assert_err("/foo/bar.rs", "foo");
            assert_err("/foo/bar.rs", "/foo/bar");
        }

        #[test]
        fn join() {
            assert_eq!(Path::new("/foo").join("bar"), PathBuf::from("/foo/bar"));
            assert_eq!(Path::new("foo").join("bar/qux"), PathBuf::from("foo/bar/qux"));
            assert_eq!(Path::new("/foo").join("..//bar"), PathBuf::from("/foo/..//bar"));
            assert_eq!(Path::new("/foo").join("/bar"), PathBuf::from("/bar"));
        }

        #[test]
        fn make_absolute() {
            fn assert_result(base: &str, path: &str, expected: &str) {
                let base = AbsolutePath::try_new(base).expect("the base path must be absolute");
                let expected = AbsolutePath::try_from(expected).expect("the expected path must be absolute");
                assert_eq!(Path::new(path).make_absolute(&base), expected);
            }

            assert_result("/foo", "", "/foo");
            assert_result("/foo", "bar", "/foo/bar");
            assert_result("/foo", "/bar", "/bar");
            assert_result("/foo", "bar/qux", "/foo/bar/qux");
            assert_result("/foo", "bar/../qux", "/foo/qux");
            assert_result("/foo", "..//bar", "/bar");
            assert_result("/foo", "../../../../bar", "/bar");
        }
    }

    mod pathbuf {
        use super::*;

        #[test]
        fn push() {
            fn assert_push(start: &str, add: &str, expected: &str) {
                let mut path = PathBuf::from(start);
                path.push(add);
                assert_eq!(path, PathBuf::from(expected));
            }

            assert_push("/foo", "bar", "/foo/bar");
            assert_push("foo", "bar/qux", "foo/bar/qux");
            assert_push("/foo", "..//bar/", "/foo/..//bar/");
            assert_push("/foo", "/bar", "/bar");
        }

        #[test]
        fn pop() {
            fn assert_pop(start: &str, expected: &str) {
                let mut path = PathBuf::from(start);
                assert!(path.pop());
                assert_eq!(path, PathBuf::from(expected));
            }

            assert_pop("/foo/bar", "/foo");
            assert_pop("foo/bar/qux", "foo/bar");
            assert_pop("/foo/..//bar", "/foo/..");
            assert_pop("/bar", "/");

            assert!(!PathBuf::from("/").pop());
        }

        #[test]
        fn set_file_name() {
            fn assert_name(start: &str, set: &str, expected: &str) {
                let mut path = PathBuf::from(start);
                path.set_file_name(set);
                assert_eq!(path, PathBuf::from(expected));
            }

            assert_name("/foo/bar", "bar.rs", "/foo/bar.rs");
            assert_name("/foo/bar.rs", "bar", "/foo/bar");
            assert_name("/foo/bar/", "bar", "/foo/bar");
            assert_name("/foo/bar", "bar/", "/foo/bar/");

            assert_name("/foo/bar", "/.bar/", "/.bar/");
        }

        #[test]
        fn set_extension() {
            fn assert_extension(start: &str, set: &str, expected: &str) {
                let mut path = PathBuf::from(start);
                path.set_extension(set);
                assert_eq!(path, PathBuf::from(expected));
            }

            assert_extension("/foo/bar", "rs", "/foo/bar.rs");
            assert_extension("/foo/bar", "tar.gz", "/foo/bar.tar.gz");

            assert_extension("/foo/bar.rs", "tar.gz", "/foo/bar.tar.gz");
            assert_extension("/foo/bar.tar.gz", "rs", "/foo/bar.tar.rs");

            assert_extension("/foo/bar.rs", "", "/foo/bar");
            assert_extension("/foo/bar.tar.gz", "", "/foo/bar.tar");
        }
    }
}
