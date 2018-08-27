compiler:
	jbuilder build @runlangc

clean:
	jbuilder clean

test:
	jbuilder build @runtest && _build/default/test/test.exe

all: compiler test 

.PHONY: compiler test all
