#include <stdlib.h>

/** Force malloc() to be linked in our executable.
 *
 * GDB requires the malloc() function in order to allocate buffers for output
 * commands like `echo` and `printf`.
 */
__attribute__((optimize("O0")))
void _force_malloc() {
    malloc(0);
}
