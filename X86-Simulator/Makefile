DIRS := util,grading,x86
LIBS := unix,str,nums
SUBMIT := simulator.ml extratests.ml
OTHER := gradedtests.ml main.ml 
HWNAME := X86-Simulator
TIMESTAMP := $(shell /bin/date "+%Y-%m-%d-%H:%M:%S")
ZIPNAME := $(HWNAME)-submit($(TIMESTAMP)).zip

.PHONY: all test clean

main.native: $(SUBMIT) $(OTHER)
	ocamlbuild -Is $(DIRS) -libs $(LIBS) main.native

main.byte: $(SUBMIT) $(OTHER)
	ocamlbuild -Is $(DIRS) -libs $(LIBS) main.byte

all: main.byte main.native

test: main.native
	./main.native --test


clean:
	ocamlbuild -clean && rm -f main.native
