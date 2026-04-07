.PHONY: all
all: helloimage

helloimage: helloimage.cpp
	g++-13 -std=c++17 -g -I/usr/include/qt6 -o helloimage helloimage.cpp -L/usr/lib64 -lQt6Core -lQt6Gui

.PHONY: clean
clean:
	rm -f helloimage helloimage.o

