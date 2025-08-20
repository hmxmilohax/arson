// SPDX-License-Identifier: LGPL-3.0-or-later

use std::ops::Range;

use crate::prelude::*;
use crate::{ArrayDisplay, ArrayDisplayOptions, ArrayKind, NumericError};

/// A contiguous, growable collection of [`Node`]s.
#[derive(Clone, Default, PartialEq, PartialOrd)]
pub struct NodeArray {
    nodes: Vec<Node>,
}

impl NodeArray {
    pub const fn new() -> Self {
        Self { nodes: Vec::new() }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self { nodes: Vec::with_capacity(capacity) }
    }

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Array, options)
    }
}

// Vec forwards
impl NodeArray {
    pub fn capacity(&self) -> usize {
        self.nodes.capacity()
    }

    pub fn push<N: Into<Node>>(&mut self, value: N) {
        self.nodes.push(value.into())
    }

    pub fn pop(&mut self) -> Option<Node> {
        self.nodes.pop()
    }

    pub fn insert<N: Into<Node>>(&mut self, index: usize, value: N) -> crate::Result {
        arson_assert!(index <= self.nodes.len());
        self.nodes.insert(index, value.into());
        Ok(())
    }

    pub fn insert_slice(&mut self, index: usize, values: &[Node]) -> crate::Result {
        self.splice(index..index, values.iter().cloned())?;
        Ok(())
    }

    pub fn remove(&mut self, index: usize) -> crate::Result<Node> {
        arson_assert!(index < self.nodes.len());
        Ok(self.nodes.remove(index))
    }

    pub fn remove_item(&mut self, value: &Node) {
        if let Some(pos) = self.nodes.iter().position(|v| v == value) {
            self.nodes.remove(pos);
        }
    }

    pub fn drain<R>(&mut self, range: R) -> crate::Result<std::vec::Drain<'_, Node>>
    where
        R: std::ops::RangeBounds<usize>,
    {
        check_range(range, self.nodes.len()).map(|range| self.nodes.drain(range))
    }

    pub fn append(&mut self, other: &mut Self) {
        self.nodes.append(&mut other.nodes)
    }

    pub fn extend_from_slice(&mut self, other: &[Node]) {
        self.nodes.extend_from_slice(other)
    }

    pub fn splice<R, I>(
        &mut self,
        range: R,
        replace_with: I,
    ) -> crate::Result<std::vec::Splice<'_, I::IntoIter>>
    where
        R: std::ops::RangeBounds<usize>,
        I: IntoIterator<Item = Node>,
    {
        check_range(range, self.nodes.len()).map(|range| self.nodes.splice(range, replace_with))
    }

    pub fn clear(&mut self) {
        self.nodes.clear()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.nodes.reserve(additional)
    }

    pub fn reserve_exact(&mut self, additional: usize) {
        self.nodes.reserve_exact(additional)
    }

    pub fn resize(&mut self, index: usize, value: Node) {
        self.nodes.resize(index, value)
    }

    pub fn resize_with<F: FnMut() -> Node>(&mut self, index: usize, value: F) {
        self.nodes.resize_with(index, value)
    }

    pub fn truncate(&mut self, len: usize) {
        self.nodes.truncate(len)
    }

    pub fn shrink_to(&mut self, min_capacity: usize) {
        self.nodes.shrink_to(min_capacity)
    }

    pub fn shrink_to_fit(&mut self) {
        self.nodes.shrink_to_fit()
    }
}

// TODO: replace with std::slice::range when that stabilizes
fn check_range<R>(range: R, length: usize) -> crate::Result<Range<usize>>
where
    R: std::ops::RangeBounds<usize>,
{
    use std::ops::Bound;

    let start = match range.start_bound() {
        Bound::Included(start) => *start,
        Bound::Excluded(start) => match start.checked_add(1) {
            Some(start) => start,
            None => return Err(NumericError::Overflow.into()),
        },
        Bound::Unbounded => 0,
    };

    let end = match range.end_bound() {
        Bound::Included(end) => match end.checked_add(1) {
            Some(end) => end,
            None => return Err(NumericError::Overflow.into()),
        },
        Bound::Excluded(end) => *end,
        Bound::Unbounded => length,
    };

    if start <= end && end <= length {
        Ok(start..end)
    } else {
        Err(NumericError::slice_out_of_range(range, 0..length).into())
    }
}

// Merge/replace procedures
impl NodeArray {
    pub fn merge_tags(&mut self, source: &NodeArray) {
        for node in source {
            let NodeValue::Array(source_array) = node.unevaluated() else {
                continue;
            };
            let Ok(source_borrow) = source_array.borrow() else {
                continue;
            };
            let Some(source_tag) = source_borrow.get_opt(0) else {
                continue;
            };

            let Some(predicate) = source_tag.unevaluated().array_tag() else {
                continue;
            };

            match self.find_tag_opt(predicate) {
                Some(found) => {
                    if let Ok(mut dest_borrow) = found.borrow_mut() {
                        dest_borrow.merge_tags(&source_borrow)
                    }
                },
                None => self.push(source_array),
            }
        }
    }

    pub fn replace_tags(&mut self, source: &NodeArray) {
        // returns whether an array was found within `dest`,
        // used to determine whether or not to replace the node wholesale
        fn replace(dest: &mut NodeArray, source: &NodeArray) -> bool {
            let mut array_found = false;

            for dest_node in dest {
                let NodeValue::Array(dest_array) = dest_node.unevaluated() else {
                    continue;
                };

                array_found = true;
                let Ok(mut dest_borrow) = dest_array.borrow_mut() else {
                    continue;
                };
                let Some(dest_tag) = dest_borrow.get_opt(0) else {
                    continue;
                };

                let Some(predicate) = dest_tag.unevaluated().array_tag() else {
                    continue;
                };

                let Some(found) = source.find_tag_opt(predicate) else {
                    continue;
                };
                let Ok(source_borrow) = found.borrow() else {
                    continue;
                };

                if !replace(&mut dest_borrow, &source_borrow) {
                    drop(dest_borrow);
                    drop(source_borrow);
                    *dest_node = found.into()
                }
            }

            array_found
        }

        replace(self, source);
    }
}

impl From<Vec<Node>> for NodeArray {
    fn from(value: Vec<Node>) -> Self {
        Self { nodes: value }
    }
}

impl FromIterator<Node> for NodeArray {
    fn from_iter<T: IntoIterator<Item = Node>>(iter: T) -> Self {
        Self { nodes: Vec::from_iter(iter) }
    }
}

impl FromIterator<NodeValue> for NodeArray {
    fn from_iter<T: IntoIterator<Item = NodeValue>>(iter: T) -> Self {
        Self {
            nodes: Vec::from_iter(iter.into_iter().map(Node::from)),
        }
    }
}

impl std::iter::Extend<Node> for NodeArray {
    fn extend<T: IntoIterator<Item = Node>>(&mut self, iter: T) {
        self.nodes.extend(iter)
    }
}

impl std::ops::Deref for NodeArray {
    type Target = NodeSlice;

    fn deref(&self) -> &Self::Target {
        NodeSlice::from(&self.nodes)
    }
}

impl std::ops::DerefMut for NodeArray {
    fn deref_mut(&mut self) -> &mut Self::Target {
        NodeSlice::from_mut(&mut self.nodes)
    }
}

impl std::borrow::Borrow<NodeSlice> for NodeArray {
    fn borrow(&self) -> &NodeSlice {
        self
    }
}

impl std::borrow::BorrowMut<NodeSlice> for NodeArray {
    fn borrow_mut(&mut self) -> &mut NodeSlice {
        self
    }
}

impl std::borrow::Borrow<Vec<Node>> for NodeArray {
    fn borrow(&self) -> &Vec<Node> {
        &self.nodes
    }
}

impl std::borrow::BorrowMut<Vec<Node>> for NodeArray {
    fn borrow_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }
}

impl AsRef<Vec<Node>> for NodeArray {
    fn as_ref(&self) -> &Vec<Node> {
        &self.nodes
    }
}

impl AsMut<Vec<Node>> for NodeArray {
    fn as_mut(&mut self) -> &mut Vec<Node> {
        &mut self.nodes
    }
}

impl IntoIterator for NodeArray {
    type Item = <Vec<Node> as IntoIterator>::Item;
    type IntoIter = <Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.nodes.into_iter()
    }
}

impl<'nodes> IntoIterator for &'nodes NodeArray {
    type Item = <&'nodes Vec<Node> as IntoIterator>::Item;
    type IntoIter = <&'nodes Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
        (&self.nodes).into_iter()
    }
}

impl<'nodes> IntoIterator for &'nodes mut NodeArray {
    type Item = <&'nodes mut Vec<Node> as IntoIterator>::Item;
    type IntoIter = <&'nodes mut Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        #[expect(
            clippy::into_iter_on_ref,
            reason = "intentionally forwarding to into_iter"
        )]
        (&mut self.nodes).into_iter()
    }
}

impl std::fmt::Debug for NodeArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nodes.fmt(f)
    }
}

impl std::fmt::Display for NodeArray {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Array).fmt(f)
    }
}

macro_rules! define_array_wrapper {
    (
        $(
            $(#[$attr:meta])*
            struct $name:ident;
        )+
    ) => {
        $(
            $(#[$attr])*
            #[derive(Clone, Default, PartialEq, PartialOrd)]
            pub struct $name {
                nodes: NodeArray,
            }

            impl $name {
                pub const fn new() -> Self {
                    Self { nodes: NodeArray::new() }
                }
            }

            impl<T> From<T> for $name
                where NodeArray: From<T>
            {
                fn from(value: T) -> Self {
                    Self { nodes: NodeArray::from(value) }
                }
            }

            impl<T> FromIterator<T> for $name
                where NodeArray: FromIterator<T>
            {
                fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
                    Self { nodes: NodeArray::from_iter(iter) }
                }
            }

            impl std::ops::Deref for $name {
                type Target = NodeArray;

                fn deref(&self) -> &Self::Target {
                    &self.nodes
                }
            }

            impl std::ops::DerefMut for $name {
                fn deref_mut(&mut self) -> &mut Self::Target {
                    &mut self.nodes
                }
            }

            impl std::borrow::Borrow<NodeArray> for $name {
                fn borrow(&self) -> &NodeArray {
                    &self.nodes
                }
            }

            impl IntoIterator for $name {
                type Item = <NodeArray as IntoIterator>::Item;
                type IntoIter = <NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    self.nodes.into_iter()
                }
            }

            impl<'nodes> IntoIterator for &'nodes $name {
                type Item = <&'nodes NodeArray as IntoIterator>::Item;
                type IntoIter = <&'nodes NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    (&self.nodes).into_iter()
                }
            }

            impl<'nodes> IntoIterator for &'nodes mut $name {
                type Item = <&'nodes mut NodeArray as IntoIterator>::Item;
                type IntoIter = <&'nodes mut NodeArray as IntoIterator>::IntoIter;

                fn into_iter(self) -> Self::IntoIter {
                    (&mut self.nodes).into_iter()
                }
            }
        )+
    };
}

define_array_wrapper! {
    /// A script command which can be executed.
    struct NodeCommand;

    /// A property on an object which can be manipulated.
    struct NodeProperty;
}

impl NodeCommand {
    pub fn execute(&self, context: &mut Context) -> ExecuteResult {
        context.execute(self)
    }

    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Command, options)
    }
}

impl std::fmt::Debug for NodeCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nodes.fmt(f)
    }
}

impl std::fmt::Display for NodeCommand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Command).fmt(f)
    }
}

impl NodeProperty {
    pub fn display_with_options(&self, options: ArrayDisplayOptions) -> ArrayDisplay<'_> {
        ArrayDisplay::new(&self.nodes, ArrayKind::Property, options)
    }
}

impl std::fmt::Debug for NodeProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.nodes.fmt(f)
    }
}

impl std::fmt::Display for NodeProperty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ArrayDisplay::new_default(&self.nodes, ArrayKind::Property).fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn merge_tags() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");
        let sym_asdf = context.add_required_symbol("asdf");
        let sym_jkl = context.add_required_symbol("jkl");

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        */
        #[rustfmt::skip] // using DTA-style formatting here
        let mut dest = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (10 "bar")
            )
        )
        (3 3)
        */
        #[rustfmt::skip]
        let source = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![3, 3],
        ];

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        (3 3)
        */
        #[rustfmt::skip]
        let dest_expected = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
            arson_array![3, 3],
        ];
        let source_expected = source.clone();

        dest.merge_tags(&source);
        assert_eq!(dest, dest_expected);
        assert_eq!(source, source_expected);
    }

    #[test]
    fn replace_tags() {
        let mut context = Context::new();

        let sym1 = context.add_required_symbol("sym1");
        let sym2 = context.add_required_symbol("sym2");
        let sym_asdf = context.add_required_symbol("asdf");
        let sym_jkl = context.add_required_symbol("jkl");

        /*
        (sym1 5)
        (sym2
            (asdf 100)
            (jkl 250)
            (1
                (5 "foo")
                (10 "baz")
            )
        )
        (4 4)
        */
        #[rustfmt::skip] // using DTA-style formatting here
        let mut dest = arson_array![
            arson_array![sym1.clone(), 5],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 100],
                arson_array![sym_jkl.clone(), 250],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "baz"],
                ],
            ],
            arson_array![4, 4],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (10 "bar")
            )
        )
        (3 3)
        */
        #[rustfmt::skip]
        let source = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![3, 3],
        ];

        /*
        (sym1 1)
        (sym2
            (asdf 10)
            (jkl 50)
            (1
                (5 "foo")
                (10 "bar")
            )
        )
        (4 4)
        */
        #[rustfmt::skip]
        let dest_expected = arson_array![
            arson_array![sym1.clone(), 1],
            arson_array![sym2.clone(),
                arson_array![sym_asdf.clone(), 10],
                arson_array![sym_jkl.clone(), 50],
                arson_array![1,
                    arson_array![5, "foo"],
                    arson_array![10, "bar"],
                ],
            ],
            arson_array![4, 4],
        ];
        let source_expected = source.clone();

        dest.replace_tags(&source);
        assert_eq!(dest, dest_expected);
        assert_eq!(source, source_expected);
    }
}
