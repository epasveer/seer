# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellosegv

# This rule tells make how to build hellosegv from hellosegv.cpp
hellosegv: hellosegv.cpp
	g++ -g -o hellosegv hellosegv.cpp function1.cpp

# This rule tells make to copy hellosegv to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellosegv binaries

# This rule tells make to delete hellosegv and hellosegv.o
.PHONY: clean
clean:
	rm -f hellosegv hellosegv.o function1.o

