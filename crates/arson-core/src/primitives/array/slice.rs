// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{ArrayDisplay, ArrayDisplayOptions, ArrayKind, Node, NodeArray};

/// A contiguous slice of [`Node`]s.
#[repr(transparent)]
#[derive(PartialEq, PartialOrd)]
pub struct NodeSlice {
    pub(super) nodes: [Node],
}

impl NodeSlice {
    pub fn empty() -> &'static NodeSlice {
        Self::from(&[])
    }

    pub fn from(nodes: &[Node]) -> &NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &*(nodes as *const [Node] as *const NodeSlice) }
    }

    pub fn from_mut(nodes: &mut [Node]) -> &mut NodeSlice {
        // SAFETY: NodeSlice transparently contains a [Node], so its layout is identical
        unsafe { &mut *(nodes as *mut [Node] as *mut NodeSlice) }
    }

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, options)
    }
}

// Sorting
impl NodeSlice {
    pub fn sort(&mut self) {
        self.nodes.sort_by(Node::total_cmp)
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Modified from SlicePartialOrd::partial_cmp in core::slice

        let min = self.len().min(other.len());

        let left = &self[..min];
        let right = &other[..min];

        for i in 0..min {
            match left[i].total_cmp(&right[i]) {
                std::cmp::Ordering::Equal => (),
                non_eq => return non_eq,
            }
        }

        self.len().cmp(&other.len())
    }
}

impl std::ops::Deref for NodeSlice {
    type Target = [Node];

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl std::ops::DerefMut for NodeSlice {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

impl<T> std::borrow::Borrow<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: std::borrow::Borrow<T>,
{
    fn borrow(&self) -> &T {
        std::ops::Deref::deref(self).borrow()
    }
}

impl<T> std::borrow::BorrowMut<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: std::borrow::BorrowMut<T>,
{
    fn borrow_mut(&mut self) -> &mut T {
        std::ops::DerefMut::deref_mut(self).borrow_mut()
    }
}

impl<T> AsRef<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: AsRef<T>,
{
    fn as_ref(&self) -> &T {
        std::ops::Deref::deref(self).as_ref()
    }
}

impl<T> AsMut<T> for NodeSlice
where
    T: ?Sized,
    <NodeSlice as std::ops::Deref>::Target: AsMut<T>,
{
    fn as_mut(&mut self) -> &mut T {
        std::ops::DerefMut::deref_mut(self).as_mut()
    }
}

impl ToOwned for NodeSlice {
    type Owned = NodeArray;

    fn to_owned(&self) -> Self::Owned {
        self.nodes.to_owned().into()
    }
}

impl<'slice> IntoIterator for &'slice NodeSlice {
    type Item = <&'slice [Node] as IntoIterator>::Item;
    type IntoIter = <&'slice [Node] as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
        self.nodes.into_iter()
    }
}

impl std::fmt::Debug for NodeSlice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nodes.fmt(f)
    }
}

impl std::fmt::Display for NodeSlice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Array).fmt(f)
    }
}
