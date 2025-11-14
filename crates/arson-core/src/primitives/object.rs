// SPDX-License-Identifier: LGPL-3.0-or-later

use std::any::Any;

use crate::{Context, ExecuteResult, NodeSlice};

#[non_exhaustive]
#[derive(thiserror::Error, Debug)]
pub enum ObjectError {
    #[cfg(feature = "dynamic-typenames")]
    #[error("cannot cast from {actual} to {expected}")]
    BadObjectCast { expected: &'static str, actual: &'static str },

    #[cfg(not(feature = "dynamic-typenames"))]
    #[error("cannot perform the requested typecast")]
    BadObjectCast,
}

pub type ObjectRef = std::rc::Rc<dyn Object>;

pub trait Object: Any + std::fmt::Debug {
    /// Gets the name for this object, if any.
    fn name(&self) -> Option<&str>;

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
    /// side-effect nature of argument evaluation unsound for the reasons
    /// specified above.
    fn handle(&self, context: &mut Context, msg: &NodeSlice) -> ExecuteResult;

    /// Gets the type name for this object.
    ///
    /// There is no particular reason to override this, simply leave it as-is.
    #[cfg(feature = "dynamic-typenames")]
    fn type_name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }
}

impl dyn Object {
    pub fn is<T: Any>(&self) -> bool {
        (self as &dyn Any).is::<T>()
    }

    pub fn downcast<T: Any>(&self) -> crate::Result<&T> {
        self.downcast_opt().ok_or_else(|| {
            #[cfg(feature = "dynamic-typenames")]
            return ObjectError::BadObjectCast {
                expected: std::any::type_name::<T>(),
                actual: self.type_name(),
            }
            .into();

            #[cfg(not(feature = "dynamic-typenames"))]
            return ObjectError::BadObjectCast.into();
        })
    }

    pub fn downcast_opt<T: Any>(&self) -> Option<&T> {
        (self as &dyn Any).downcast_ref()
    }

    pub fn downcast_mut<T: Any>(&mut self) -> crate::Result<&mut T> {
        #[cfg(feature = "dynamic-typenames")]
        let typename = self.type_name();

        self.downcast_mut_opt().ok_or_else(|| {
            #[cfg(feature = "dynamic-typenames")]
            return ObjectError::BadObjectCast {
                expected: std::any::type_name::<T>(),
                actual: typename,
            }
            .into();

            #[cfg(not(feature = "dynamic-typenames"))]
            return ObjectError::BadObjectCast.into();
        })
    }

    pub fn downcast_mut_opt<T: Any>(&mut self) -> Option<&mut T> {
        (self as &mut dyn Any).downcast_mut()
    }

    fn as_ptr(&self) -> *const () {
        self as *const _ as *const ()
    }

    pub fn total_cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.as_ptr() == other.as_ptr() {
            true => std::cmp::Ordering::Equal,
            #[cfg(feature = "dynamic-typenames")]
            false => match self.type_name().cmp(other.type_name()) {
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
