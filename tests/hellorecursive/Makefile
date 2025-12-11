.PHONY: all
all: hellorecursive

hellorecursive: hellorecursive.cpp
	g++ -std=c++17 -g -I/usr/include/qt6 -o hellorecursive hellorecursive.cpp -L/usr/lib64 -lQt6Widgets -lQt6Core

.PHONY: clean
clean:
	rm -f hellorecursive hellorecursive.o

