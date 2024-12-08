.PHONY: all
all: helloasm

helloasm.o: helloasm.asm
	nasm -f elf -g -F dwarf -o helloasm.o helloasm.asm

helloasm: helloasm.o
	ld -m elf_i386 -o helloasm helloasm.o

.PHONY: clean
clean:
	rm -f helloasm helloasm.o

# 	nasm -f elf32 -g -F stabs -o helloasm.o helloasm.asm
