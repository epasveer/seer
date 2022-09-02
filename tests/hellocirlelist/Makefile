# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellocirclelist

# This rule tells make how to build hellocirclelist from hellocirclelist.cpp
hellocirclelist: hellocirclelist.cpp
	g++ -g -o hellocirclelist hellocirclelist.cpp

# This rule tells make to delete hellocirclelist and hellocirclelist.o
.PHONY: clean
clean:
	rm -f hellocirclelist hellocirclelist.o

