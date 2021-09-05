# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellothreads

# This rule tells make how to build hellothreads from hellothreads.cpp
hellothreads: hellothreads.cpp
	g++ -g -pthread -o hellothreads hellothreads.cpp

# This rule tells make to copy hellothreads to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellothreads binaries

# This rule tells make to delete hellothreads and hellothreads.o
.PHONY: clean
clean:
	rm -f hellothreads hellothreads.o

