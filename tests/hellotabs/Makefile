# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellotabs

# This rule tells make how to build hellotabs from hellotabs.cpp
hellotabs: hellotabs.cpp
	g++ -g -o hellotabs hellotabs.cpp function1.cpp

# This rule tells make to delete hellotabs and hellotabs.o
.PHONY: clean
clean:
	rm -f hellotabs hellotabs.o function1.o

