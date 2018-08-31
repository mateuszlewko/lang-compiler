.PHONY: default build install uninstall test clean

default: build

build:
	jbuilder build

test:
	jbuilder runtest -f

install:
	jbuilder build @install
	jbuilder install

uninstall:
	jbuilder uninstall

clean:
	jbuilder clean