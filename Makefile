make test: dune-project
	rm -f libeuropa/libeuropa.so
	cargo build --release --manifest-path=libeuropa/Cargo.toml
	mv libeuropa/target/release/libeuropa.dylib libeuropa/libeuropa.so

	dune build

	./_build/install/default/bin/europa examples/first.eu
