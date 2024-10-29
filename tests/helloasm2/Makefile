.PHONY: all
all: square_test

square_test.o: square_test.s
	as --32 -g square_test.s -o square_test.o

isolated_square.o: isolated_square.s
	as --32 -g isolated_square.s -o isolated_square.o

square_test: square_test.o isolated_square.o
	ld -m elf_i386 -g -o square_test square_test.o isolated_square.o

.PHONY: clean
clean:
	rm -f square_test square_test.o isolated_square.o

