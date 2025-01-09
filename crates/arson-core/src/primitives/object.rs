// SPDX-License-Identifier: LGPL-3.0-or-later

use crate::{Context, ExecuteResult, ExecutionError, NodeSlice};

pub type ObjectRef = std::rc::Rc<dyn Object>;

pub trait Object: std::any::Any + std::fmt::Debug {
    /// Gets the name for this object, if any.
    fn name(&self) -> Option<&String>;

    /// Sends a message/command to the object to be handled.
    ///
    /// # A note about argument evaluation
    ///
    /// Because arguments can themselves be commands, handlers must be careful
    /// about evaluating all arguments *before* performing any actions which
    /// take some form of lock, such as a Mutex or a RefCell. A command such as
    /// `{game set_score {+ {game get_score} 50}}` is valid, and will call into
    /// `set_score` *first*. The call to `get_score` will only occur once the
    /// argument for `set_score` is evaluated inside its implementation.
    ///
    /// This is also the reason why the `self` argument here is *immutable* as
    /// opposed to *mutable*. Making `self` mutable here results in all sorts
    /// of logistical issues in implementation, and additionally makes the
    /// side-effect nature of argument evaluation less sound for the reasons
    /// specified above.
    fn handle(&self, context: &mut Context, msg: &NodeSlice) -> ExecuteResult;

    /// Interstitial workaround for [lack of trait upcasting coersion](https://github.com/rust-lang/rust/issues/65991).
    ///
    /// Simply implement as:
    ///
    /// ```rust,no_run
    /// # struct Asdf;
    /// # impl Asdf {
    /// fn as_any(&self) -> &dyn std::any::Any {
    ///     self
    /// }
    /// # }
    /// ```
    fn as_any(&self) -> &dyn std::any::Any;

    /// Gets the type name for this object.
    ///
    /// There is no particular reason to override this, simply leave it as-is.
    #[cfg(feature = "dynamic-typenames")]
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl dyn Object {
    pub fn downcast<T: 'static>(&self) -> crate::Result<&T> {
        self.downcast_opt().ok_or_else(|| {
            #[cfg(feature = "dynamic-typenames")]
            return ExecutionError::BadObjectCast {
                expected: std::any::type_name::<T>(),
                actual: self.type_name(),
            }
            .into();

            #[cfg(not(feature = "dynamic-typenames"))]
            return ExecutionError::BadObjectCast.into();
        })
    }

    pub fn downcast_opt<T: 'static>(&self) -> Option<&T> {
        self.as_any().downcast_ref()
    }

    fn as_ptr(&self) -> *const () {
        self as *const _ as *const ()
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.as_ptr() == other.as_ptr() {
            true => std::cmp::Ordering::Equal,
            #[cfg(feature = "dynamic-typenames")]
            false => match self.type_name().cmp(&other.type_name()) {
                std::cmp::Ordering::Equal => self.name().cmp(&other.name()),
                result => result,
            },
            #[cfg(not(feature = "dynamic-typenames"))]
            false => self.name().cmp(&other.name()),
        }
    }
}

impl PartialEq for dyn Object {
    fn eq(&self, other: &Self) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl PartialEq<ObjectRef> for dyn Object {
    fn eq(&self, other: &ObjectRef) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl PartialEq<dyn Object> for ObjectRef {
    fn eq(&self, other: &dyn Object) -> bool {
        self.as_ptr() == other.as_ptr()
    }
}

impl std::fmt::Display for dyn Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.name() {
            Some(name) if !name.is_empty() => f.write_str(name),
            #[cfg(feature = "dynamic-typenames")]
            _ => write!(f, "<type {}>", self.type_name()),
            #[cfg(not(feature = "dynamic-typenames"))]
            _ => f.write_str("<type Object>"),
        }
    }
}
