.PHONY: all
all: helloutl

# Needs a valid build directory to get SeerUtl.cpp.o
helloutl: helloutl.cpp
	g++ -I/usr/include/qt5 -g -o helloutl helloutl.cpp ../../src/build/CMakeFiles/seergdb.dir/SeerUtl.cpp.o -L/usr/lib64 -lQt5Core

.PHONY: clean
clean:
	rm -f helloutl helloutl.o

