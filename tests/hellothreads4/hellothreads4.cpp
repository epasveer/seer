//
// CPP program to demonstrate multithreading
//
#include <iostream>
#include <thread>
#include <unistd.h>

// A callable object
class thread_obj {
    public:
        void operator() (int x) {
            for (int i = 0; i < x; i++) {
                std::cout << "Thread using function object as callable" << std::endl;
                sleep(3);
            }
            sleep(1);
        }
};

int main() {

    // This thread is launched by using
    // function object as callable
    std::thread th3(thread_obj(), 3);
    std::thread th4(thread_obj(), 3);
    std::thread th5(thread_obj(), 3);
    std::thread th6(thread_obj(), 3);
    std::thread th7(thread_obj(), 3);
    std::thread th8(thread_obj(), 3);
    std::thread th9(thread_obj(), 3);

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

