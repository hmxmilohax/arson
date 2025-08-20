// SPDX-License-Identifier: LGPL-3.0-or-later

#[macro_export]
macro_rules! arson_array {
    () => (
        $crate::NodeArray::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::NodeArray::from(vec![$elem.into(); $n])
    );
    ($($x:expr),+ $(,)?) => (
        $crate::NodeArray::from(vec![$($x.into()),+])
    );
}

#[macro_export]
macro_rules! arson_slice {
    () => (
        $crate::NodeSlice::new()
    );
    ($elem:expr; $n:expr) => (
        $crate::NodeSlice::from(&std::array::from_fn::<_, $n, _>(|_| $elem.into()))
    );
    ($($x:expr),+ $(,)?) => (
        $crate::NodeSlice::from(&[$($x.into()),+])
    );
}
