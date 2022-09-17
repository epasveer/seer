# This is the default target, which will be built when
# you invoke make
.PHONY: all
all: hellocaller hellocallee

hellocaller: hellocaller.c
	g++ -g -o hellocaller hellocaller.c

hellocallee: hellocallee.c
	g++ -g -o hellocallee hellocallee.c

.PHONY: clean
clean:
	rm -f hellocaller hellocaller.o
	rm -f hellocallee hellocallee.o

