//! Helper module defining serialization/deserialization support for [`serde`].

pub mod de;
mod deserialize;
pub mod ser;
mod serialize;
mod str;

#[cfg(feature = "metadata")]
pub mod metadata;
