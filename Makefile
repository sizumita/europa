make test: dune-project
	dune build
	gcc -shared -fPIC ./stubs/bindings.c -o libbindings.so
	./_build/install/default/bin/europa examples/first.eu
