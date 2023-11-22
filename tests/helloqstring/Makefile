.PHONY: all
all: helloqstring

helloqstring: helloqstring.cpp
	g++ -std=c++17 -g -I/usr/include/qt6 -o helloqstring helloqstring.cpp -L/usr/lib64 -lQt6Core

.PHONY: clean
clean:
	rm -f helloqstring helloqstring.o

