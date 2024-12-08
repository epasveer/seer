
Steps to compile an asm program.

    % nasm -f elf -F stabs helloasm.asm -o helloasm.o
    % ld -m elf_i386 helloasm.o -o helloasm
    % seergdb -s --sat yes --bf _start helloasm

Find address to set start breakpoint.

    % objdump -f helloasm

    helloasm:     file format elf32-i386
    architecture: i386, flags 0x00000112:
    EXEC_P, HAS_SYMS, D_PAGED
    start address 0x08048080

Add a way to provide the breakpoint address/function in the
debug dialog and start command line.

Typical _start address is: 0x8048080


In gdb, to disassemble a range of memmory. 20 bytes before an
address to 20 bytes after.

    (gdb) disassemble 0x08048080-20, +20

