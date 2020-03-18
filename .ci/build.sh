#!/bin/bash

set -ex

cargo build --verbose
cargo test --verbose

if [ "$TRAVIS_RUST_VERSION" = "nightly" ]
then
    cargo build --verbose --features no_stdlib
    cargo test --verbose --features no_stdlib
fi

