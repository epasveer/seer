.PHONY: all
all: helloqtregularexpression

helloqtregularexpression: helloqtregularexpression.cpp
	g++ -std=c++17 -g -I/usr/include/qt5 -o helloqtregularexpression helloqtregularexpression.cpp -L/usr/lib64 -lQt5Core

.PHONY: clean
clean:
	rm -f helloqtregularexpression helloqtregularexpression.o

