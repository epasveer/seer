#include <iostream>
#include "arithmetic.h"
#include "advanced.h"
#include <chrono>
#include <thread>
#include <unistd.h>
using namespace std;


struct familyStruct globalStruct = {
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
int main() {
    globalStruct.numberOfNumber =0;
    int a = 10, b = 3;
    pid_t pid = getpid();
    std::cout << "Process ID: " << pid << std::endl;
    while (globalStruct.numberOfNumber < 100)
    {
        globalStruct.numberOfNumber++;
        globalStruct.dad.age = globalStruct.numberOfNumber;
        globalStruct.dad.name = "Dad String " + std::to_string(globalStruct.numberOfNumber);
        cout << "Basic Arithmetic:" << endl;
        cout << a << " + " << b << " = " << add(a, b) << endl;
        cout << a << " - " << b << " = " << subtract(a, b) << endl;
        cout << a << " * " << b << " = " << multiply(a, b) << endl;
        cout << a << " / " << b << " = " << divide(a, b) << endl;

        cout << "\nAdvanced Math:" << endl;
        cout << "Factorial of 5 = " << factorial(5) << endl;
        cout << "Fibonacci of 7 = " << fibonacci(7) << endl;
        globalStruct.dad.age ++;
        std::this_thread::sleep_for(std::chrono::seconds(2));
    }
    return 0;
}