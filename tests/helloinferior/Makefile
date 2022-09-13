# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: helloinferior

helloinferior: helloinferior.c
	g++ -g -o helloinferior helloinferior.c

.PHONY: clean
clean:
	rm -f helloinferior helloinferior.o

