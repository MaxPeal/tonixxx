BIN=bin/bronze
VERSION=0.0.1

all: $(BIN)

$(BIN): src/bin/*.rs src/lib/*.rs
	cargo build --release
	mkdir -p bin
	cp target/release/bronze bin

test: src/lib/*.rs
	cargo test

clean:
	-rm -rf bin
	-rm -rf target
	-rm Cargo.lock

clippy: src/bin/*.rs src/lib/*.rs
	cargo +nightly clippy

lint: clippy
