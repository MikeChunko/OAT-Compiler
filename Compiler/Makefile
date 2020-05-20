INCLUDES= util,x86,grading,ll
LIBS = unix,str,nums

TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")

all: main.native

.PHONY: test
test: main.native
	./main.native --test

.PHONY: main.native
main.native: 
	ocamlbuild -Is $(INCLUDES) -libs $(LIBS) main.native -use-menhir -yaccflag --explain

.PHONY: printanalysis.native
printanalysis.native: 
	ocamlbuild -Is $(INCLUDES) -libs $(LIBS) printanalysis.native -use-menhir -yaccflag --explain

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -rf output a.out
