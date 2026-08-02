# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloscatter

# This rule tells make how to build helloscatter from helloscatter.cpp
helloscatter: helloscatter.cpp
	g++ -g -o helloscatter helloscatter.cpp

# This rule tells make to copy helloscatter to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloscatter binaries

# This rule tells make to delete helloscatter
.PHONY: clean
clean:
	rm -f helloscatter helloscatter.o
