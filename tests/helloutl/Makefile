.PHONY: all
all: test_utl

# Needs a valid build directory to get SeerUtl.cpp.o
test_utl: test_utl.cpp
	g++ -I/usr/include/qt5 -g -o test_utl test_utl.cpp ../../src/build/CMakeFiles/seergdb.dir/SeerUtl.cpp.o -L/usr/lib64 -lQt5Core

.PHONY: clean
clean:
	rm -f test_utl test_utl.o

