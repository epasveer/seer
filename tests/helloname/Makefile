# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloname

# This rule tells make how to build helloname from helloname.cpp
helloname: helloname.cpp
	g++ -g -o helloname helloname.cpp

# This rule tells make to copy helloname to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p helloname binaries

# This rule tells make to delete helloname and helloname.o
.PHONY: clean
clean:
	rm -f helloname helloname.o

