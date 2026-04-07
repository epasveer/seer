# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellostruct

# This rule tells make how to build hellostruct from hellostruct.cpp
hellostruct: hellostruct.cpp
	g++-14 -g -o hellostruct hellostruct.cpp

# This rule tells make to copy hellostruct to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellostruct binaries

# This rule tells make to delete hellostruct and hellostruct.o
.PHONY: clean
clean:
	rm -f hellostruct hellostruct.o

