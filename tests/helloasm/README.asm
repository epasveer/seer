
Steps to compile an asm program.

% nasm -f elf -F stabs helloasm.asm -o helloasm_stabs.o
% ld -m elf_i386 helloasm_stabs.o -o helloasm_stabs
% seer -s helloasm_stabs

