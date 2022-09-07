
Steps to compile an asm program.

    % nasm -f elf -F stabs helloasm.asm -o helloasm_stabs.o
    % ld -m elf_i386 helloasm_stabs.o -o helloasm_stabs
    % seergdb -s --sat yes --bf _start helloasm_stabs

Find address to set start breakpoint.

    % objdump -f helloasm_stabs

    helloasm_stabs:     file format elf32-i386
    architecture: i386, flags 0x00000112:
    EXEC_P, HAS_SYMS, D_PAGED
    start address 0x08048080

Add a way to provide the breakpoint address/function in the
debug dialog and start command line.

Typical _start address is: 0x8048080



