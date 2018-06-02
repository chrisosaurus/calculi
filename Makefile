.POSIX:

all: run

build:
	stack build

run: build
	stack test

test: build
	stack test

clean:
	stack clean
	rm -rf .stack-work

.PHONY: all build clean run test
