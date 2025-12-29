# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellothreads2

# This rule tells make how to build hellothreads2 from hellothreads2.cpp
hellothreads2: hellothreads2.cpp
	g++ -g -pthread -o hellothreads2 hellothreads2.cpp

# This rule tells make to copy hellothreads2 to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellothreads2 binaries

# This rule tells make to delete hellothreads2 and hellothreads2.o
.PHONY: clean
clean:
	rm -f hellothreads2 hellothreads2.o Digraph* DirectedGraph*

