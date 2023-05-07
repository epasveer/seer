#include <stdio.h>
#include <functional>

int successor (int i) { return i + 1; }

int apply0 (int (*fn)(int), int arg)
{
  return fn (arg);
}

int apply1 (std::function<int(int)> fn, int arg)
{
  return fn (arg);
}

std::function<int(int)> make_function(int& x) {
    return [&] (int i) { return i + x; };               /* Line 17 */
}

int main (int argc, char **argv)
{
  int n = 7, m = -28;

  printf ("Answer 1 is %d\n", apply0 (successor, 3));   /* Line 24 */
  printf ("Answer 2 is %d\n", apply1 (successor, 4));   /* Line 25 */

  printf ("Answer 3 is %d\n",
          apply0 ([] (int i) { return i + 1; }, 1));    /* Line 28 */
  printf ("Answer 4 is %d\n",
          apply1 ([] (int i) { return i + 1; }, 2));    /* Line 30 */

  printf ("Answer 5 is %d\n",
          apply1 ([n] (int i) { return i + n; }, 4));   /* Line 33 */

  auto lf2 = make_function (n);                         /* Line 35 */
  printf ("Answer 6 is %d\n", apply1 (lf2, 1));         /* Line 36 */

  auto lf3 = make_function (m);
  printf ("Answer 7 is %d\n", apply1 (lf3, -14));       /* Line 39 */

  return 0;
}

//
// Keep line numbers to match this article about debugging lambdas.
// https://developers.redhat.com/articles/2023/05/03/how-debug-c-lambda-expressions-gdb#
//

