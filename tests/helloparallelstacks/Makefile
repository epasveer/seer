.PHONY: all
all: helloparallelstacks

helloparallelstacks: helloparallelstacks.cpp
	g++-15 -std=c++23 -g -pthread -o helloparallelstacks helloparallelstacks.cpp

.PHONY: clean
clean:
	rm -f helloparallelstacks helloparallelstacks.o Digraph* DirectedGraph*

