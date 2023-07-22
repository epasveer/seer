.PHONY: all
all: hellosanitizer

hellosanitizer: hellosanitizer.cpp
	g++ -g -fsanitize=address -o hellosanitizer hellosanitizer.cpp

.PHONY: clean
clean:
	rm -f hellosanitizer hellosanitizer.o

