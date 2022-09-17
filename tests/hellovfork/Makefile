# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellovfork

hellovfork: hellovfork.c
	g++ -g -o hellovfork hellovfork.c

.PHONY: clean
clean:
	rm -f hellovfork hellovfork.o

