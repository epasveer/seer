//
// CPP program to demonstrate multithreading
// using three different callables.
//
#include <iostream>
#include <thread>
#include <unistd.h>

// A dummy function
void foo(int Z) {

    for (int i = 0; i < Z; i++) {
        std::cout << "Thread using function pointer as callable" << std::endl;
        sleep(2);
    }
    sleep(5);
}

// A callable object
class thread_obj {
    public:
        void operator()(int x) {
            for (int i = 0; i < x; i++) {
                std::cout << "Thread using function object as  callable" << std::endl;
                sleep(3);
            }
            sleep(1);
        }
};

int main() {

    std::cout << "Threads 1 and 2 and 3 operating independently" << std::endl;

    // This thread is launched by using
    // function pointer as callable
    std::thread th1(foo, 3);

    // This thread is launched by using
    // function object as callable
    std::thread th2(thread_obj(), 3);
    std::thread th4(thread_obj(), 3);
    std::thread th5(thread_obj(), 3);
    std::thread th6(thread_obj(), 3);
    std::thread th7(thread_obj(), 3);
    std::thread th8(thread_obj(), 3);
    std::thread th9(thread_obj(), 3);

    // Define a Lambda Expression
    auto f = [](int x) {
        for (int i = 0; i < x; i++) {
            std::cout << "Thread using lambda expression as callable" << std::endl;
            sleep(1);
        }
        sleep(10);
    };

    // This thread is launched by using
    // lamda expression as callable
    std::thread th3(f, 3);

    // Wait for the threads to finish
    // Wait for thread t1 to finish
    th1.join();

    // Wait for thread t2 to finish
    th2.join();

    // Wait for thread t3 to finish
    th3.join();
    th4.join();
    th5.join();
    th6.join();
    th7.join();
    th8.join();
    th9.join();

    return 0;
}

