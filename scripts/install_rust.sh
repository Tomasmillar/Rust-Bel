#!/bin/sh
echo "Installing Rust via rustup..."
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
echo "Done. Loading cargo into current shell..."
. "$HOME/.cargo/env"
cargo --version
rustc --version
