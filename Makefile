.PHONY: all clean utop build run

all: main.exe

%.exe: src/main.ml
	dune build
	cp src/$@ .
	rm src/$@

.FORCE:

build:
	dune build --auto-promote

clean:
	dune clean
	rm -rf *.exe

utop: main.exe
	dune utop

run: 
	make clean 
	make 
	dune exec ./main.exe code.fpl