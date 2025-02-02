.PHONY: all
all: helloworld triangle

helloworld: helloworld.cpp
	g++ -g -o helloworld helloworld.cpp -lncurses

triangle: triangle.cpp
	g++ -g -o triangle triangle.cpp -lncurses

.PHONY: clean
clean:
	rm -f helloworld triangle

