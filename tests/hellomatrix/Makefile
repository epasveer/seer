# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellomatrix

# This rule tells make how to build hellomatrix from hellomatrix.cpp
hellomatrix: hellomatrix.cpp
	g++ -g -o hellomatrix hellomatrix.cpp

# This rule tells make to copy hellomatrix to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellomatrix binaries

# This rule tells make to delete hellomatrix
.PHONY: clean
clean:
	rm -f hellomatrix hellomatrix.o
