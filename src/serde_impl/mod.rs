//! Helper module defining serialization/deserialization support for [`serde`].

pub mod de;
pub mod ser;
mod str;

#[cfg(feature = "metadata")]
pub mod metadata;
