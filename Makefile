.PHONY: all clean utop build run

all: main.exe

%.exe: src/main.ml
	dune build
	cp src/$@ .
	rm src/$@

.FORCE:

build:
	dune build

build-dev:
	dune build
	cp _build/default/src/main.exe combc 
	chmod +w combc

build-release:
	dune build
	cp _build/release-unix/src/main.exe combc 
	chmod +w combc

clean:
	dune clean
	rm -f combc
	rm -rf *.exe

utop: main.exe
	dune utop


generate-messages:
	dune build @generate-parser-messages


update-messages: 
	dune build @update-parser-messages

run: 
	dune exec --context=default combc code.fpl

run-release:
	dune exec --context=release-unix combc code.fpl

run-js:
	dune exec --context=release-js combc code.fpl