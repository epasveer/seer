.PHONY: all
all: hellodlopen function1.so

function1.so: function1.cpp function1.o
	g++ -c -g -fpic -o function1.o function1.cpp
	g++ -shared -o function1.so function1.o

hellodlopen: hellodlopen.cpp
	g++ -g -o hellodlopen hellodlopen.cpp -ldl

.PHONY: clean
clean:
	rm -f hellodlopen hellodlopen.o function1.o function1.so

