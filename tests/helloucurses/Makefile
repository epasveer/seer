.PHONY: all
all: helloucurses

helloucurses: helloucurses.c
	gcc-13 -g -o helloucurses helloucurses.c -I/usr/local/uCurses -L /usr/local/lib64 -lm -luCurses

.PHONY: clean
clean:
	rm -f helloucurses helloucurses.o

