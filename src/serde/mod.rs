//! _(SERDE)_ Serialization and deserialization support for [`serde`](https://crates.io/crates/serde).
//! Exported under the `serde` feature only.

mod de;
mod deserialize;
mod ser;
mod serialize;
mod str;

#[cfg(feature = "metadata")]
#[cfg(feature = "serde")]
mod metadata;

pub use de::from_dynamic;
pub use ser::to_dynamic;
