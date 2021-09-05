# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloarray

# This rule tells make how to build helloarray from helloarray.cpp
helloarray: helloarray.cpp
	g++ -g -o helloarray helloarray.cpp

# This rule tells make to copy helloarray to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloarray binaries

# This rule tells make to delete helloarray and helloarray.o
.PHONY: clean
clean:
	rm -f helloarray helloarray.o

