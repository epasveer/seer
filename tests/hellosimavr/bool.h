/**
 * \file bool.h
 *
 * a simple replacement for stdbool.h. simavr and/or simulavr + gdb does not
 * like the data types described in stdbool.h, so we use this instead.
 */
#ifndef _bool_h
#define _bool_h

typedef enum bool {false=0, true} bool;

#endif // _bool_h
