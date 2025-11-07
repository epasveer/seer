#include "advanced.h"
/***
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 */
int factorial(int n) {
    struct structTetst testStruct = {-2, "test", {-1, "childTest"}};
    testStruct.id ++;
    testStruct.id ++;
    testStruct.id ++;
    int abc = 0;
    abc ++;
    abc *= 2;
    testStruct.child.childId ++;
    testStruct.child.childString = "childTest " + std::to_string(abc);
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
/***
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 * 
 */
int fibonacci(int n) {
    if (n <= 1) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}
