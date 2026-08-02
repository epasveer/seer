.PHONY: all
all: hellothreads5

hellothreads5: hellothreads5.cpp
	g++-15 -std=c++23 -g -pthread -o hellothreads5 hellothreads5.cpp

.PHONY: clean
clean:
	rm -f hellothreads5 hellothreads5.o Digraph* DirectedGraph*

