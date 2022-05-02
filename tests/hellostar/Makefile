# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellostar

# This rule tells make how to build hellostar from hellostar.cpp
hellostar: hellostar.cpp
	g++ -g -o hellostar hellostar.cpp

# This rule tells make to copy hellostar to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellostar binaries

# This rule tells make to delete hellostar and hellostar.o
.PHONY: clean
clean:
	rm -f hellostar hellostar.o

