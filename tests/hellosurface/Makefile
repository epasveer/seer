# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellosurface

# This rule tells make how to build hellosurface from hellosurface.cpp
hellosurface: hellosurface.cpp
	g++ -g -o hellosurface hellosurface.cpp

# This rule tells make to copy hellosurface to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellosurface binaries

# This rule tells make to delete hellosurface and hellosurface.o
.PHONY: clean
clean:
	rm -f hellosurface hellosurface.o

