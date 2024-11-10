# This is the default target, which will be built when you invoke make
.PHONY: all
all: helloworld

# This rule tells make how to build the test program
helloworld: helloworld.cpp
	g++ -g -o helloworld helloworld.cpp function1.cpp valueoflife.cpp

# This rule tells make to clean things
.PHONY: clean
clean:
	rm -f helloworld helloworld.o function1.o valueoflife.o

