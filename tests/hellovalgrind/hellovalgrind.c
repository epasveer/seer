#include <stdio.h>
#include <stdlib.h>

// https://developers.redhat.com/articles/2021/11/01/debug-memory-errors-valgrind-and-gdb#using_valgrind_and_gdb_together

typedef struct foo {
    int flag1;
    int flag2;
    int size;
    int **buf;
} foo;

void print_buf(struct foo *s)
{

   printf("buf[0][0]: %d\n", s->buf[0][0]);
   free(s->buf);
}

void setup_foo(struct foo *s)
{
    s->flag2 = 2;
    s->buf = malloc(20 * sizeof(int));
    for (int i = 0; i < 20; i++)
        s->buf[i] = malloc(20 * sizeof(int));
}

int main(void)
{
   struct foo s;

   setup_foo(&s);
   print_buf(&s);

   if (s.flag1 || s.flag2)
       printf("hey\n");

   return 0;
}

