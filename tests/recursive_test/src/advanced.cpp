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
    struct familyStruct family = {
        // DAD
        {
            50,
            "DAD_NAME",
            {
                {1,
                "1st_child"},
                {2,
                "2nd_child"},
                {3,
                "3rd_child"},
            }
        },
        4, "FAMILY", 
        
        // MOM
        {
            45,
            "MOM_NAME",
            {
                {'a',
                "a_child"},
                {'b',
                "b_child"},
                {'c',
                "c_child"},
            }

        }
    };
    family.numberOfNumber ++;
    family.numberOfNumber ++;
    family.numberOfNumber ++;
    int abc = 0;
    abc ++;
    abc *= 2;
    family.dad.childArr[0].age ++;
    family.dad.childArr[1].name = "childTest " + std::to_string(abc);
    family.mom.childArr[1].age ++;
    family.mom.childArr[0].name = "childTest " + std::to_string(abc) + std::to_string(abc);
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
