# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellostart

# This rule tells make how to build hellostart from hellostart.cpp
hellostart: hellostart.cpp
	g++ -g -o hellostart hellostart.cpp

# This rule tells make to copy hellostart to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellostart binaries

# This rule tells make to delete hellostart and hellostart.o
.PHONY: clean
clean:
	rm -f hellostart hellostart.o

