compiler:
	jbuilder build @runlangc

clean:
	jbuilder clean

test:
	jbuilder build @runtest && _build/default/test/test.exe

benchmark:
	./benchmarks/build-all-and-run.sh

all: compiler test 

.PHONY: compiler test all
