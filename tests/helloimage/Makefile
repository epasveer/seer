.PHONY: all
all: helloimage

helloimage: helloimage.cpp
	g++ -std=c++17 -g -I/usr/include/qt5 -o helloimage helloimage.cpp -L/usr/lib64 -lQt5Core -lQt5Gui

.PHONY: clean
clean:
	rm -f helloimage helloimage.o

