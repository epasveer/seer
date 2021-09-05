# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloebcdic

# This rule tells make how to build helloebcdic from helloebcdic.cpp
helloebcdic: helloebcdic.cpp
	g++ -g -o helloebcdic helloebcdic.cpp

# This rule tells make to copy helloebcdic to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloebcdic binaries

# This rule tells make to delete helloebcdic and helloebcdic.o
.PHONY: clean
clean:
	rm -f helloebcdic helloebcdic.o

