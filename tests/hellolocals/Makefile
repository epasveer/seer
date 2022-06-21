# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellolocals

# This rule tells make how to build hellolocals from hellolocals.cpp
hellolocals: hellolocals.cpp
	g++ -g -o hellolocals hellolocals.cpp

# This rule tells make to delete hellolocals and hellolocals.o
.PHONY: clean
clean:
	rm -f hellolocals hellolocals.o

