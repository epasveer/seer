# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellostatic

# This rule tells make how to build hellostatic from hellostatic.cpp
hellostatic: hellostatic.cpp
	g++ -g -o hellostatic hellostatic.cpp

# This rule tells make to delete hellostatic and hellostatic.o
.PHONY: clean
clean:
	rm -f hellostatic hellostatic.o

