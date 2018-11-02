#!/bin/sh
rustup update stable &&
    rustup default stable &&
    rustup component add clippy-preview &&
    cargo install tinyrick
