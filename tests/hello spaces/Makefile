# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloworld

# This rule tells make how to build helloworld from helloworld.cpp
helloworld: helloworld.cpp
	g++ -g -o helloworld helloworld.cpp function1.cpp

# This rule tells make to copy helloworld to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloworld binaries

# This rule tells make to delete helloworld and helloworld.o
.PHONY: clean
clean:
	rm -f helloworld helloworld.o function1.o

