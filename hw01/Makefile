DIRS := util,grading
SUBMIT := hellocaml.ml providedtests.ml
HWNAME := hw01
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip

.PHONY: all test clean zip

main.native: $(SUBMIT)
	ocamlbuild -Is $(DIRS) -libs unix,str main.native

main.byte:
	ocamlbuild -Is $(DIRS) -libs unix,str main.byte

all: main.native

test: main.native
	./main.native --test

zip: $(SUBMIT)
	zip '$(ZIPNAME)' $(SUBMIT)


clean:
	ocamlbuild -clean
