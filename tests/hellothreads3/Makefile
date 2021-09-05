# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellothreads3

# This rule tells make how to build hellothreads3 from hellothreads3.cpp
hellothreads3: hellothreads3.cpp
	g++ -g -pthread -o hellothreads3 hellothreads3.cpp

# This rule tells make to copy hellothreads3 to the binaries subdirectory,
# creating it if necessary
.PHONY: install
install:
	mkdir -p binaries
	cp -p hellothreads3 binaries/

# This rule tells make to delete hellothreads3 and hellothreads3.o
.PHONY: clean
clean:
	rm -f hellothreads3 hellothreads3.o

