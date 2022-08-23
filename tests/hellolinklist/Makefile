# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellolinklist

# This rule tells make how to build hellolinklist from hellolinklist.cpp
hellolinklist: hellolinklist.cpp
	g++ -g -o hellolinklist hellolinklist.cpp

# This rule tells make to delete hellolinklist and hellolinklist.o
.PHONY: clean
clean:
	rm -f hellolinklist hellolinklist.o

