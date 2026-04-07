# file name: isolated_square.s

        .section .data

        .section .text

        .globl square

        .type square,@function

square:
        pushl %ebp
        movl %esp, %ebp
        movl 8(%ebp), %eax

        imull %eax, %eax

end_square:
        movl %ebp, %esp
        popl %ebp
        ret

