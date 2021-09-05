# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloonefile

# This rule tells make how to build helloonefile from helloonefile.cpp
helloonefile: helloonefile.cpp
	g++ -g -o helloonefile helloonefile.cpp

# This rule tells make to copy helloonefile to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloonefile binaries

# This rule tells make to delete helloonefile and helloonefile.o
.PHONY: clean
clean:
	rm -f helloonefile helloonefile.o

