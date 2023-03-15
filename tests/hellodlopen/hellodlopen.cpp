#include <iostream>
#include <dlfcn.h>

int main() {

    std::cout << "C++ dlopen demo" << std::endl << std::endl;

    // open the library
    std::cout << "Opening function1.so..." << std::endl;

    void* handle = dlopen("./function1.so", RTLD_LAZY);
    if (handle == NULL) {
        std::cout << "Cannot open library: " << dlerror() << std::endl;
        return 1;
    }

    // load the symbol
    std::cout << "Loading symbol function1..." << std::endl;

    typedef void (*function1_t)();

    // reset errors
    dlerror();

    function1_t function1 = (function1_t) dlsym(handle, "function1");

    const char *dlsym_error = dlerror();
    if (dlsym_error) {
        std::cout << "Cannot load symbol 'function1': " << dlsym_error << std::endl;

        dlclose(handle);

        return 1;
    }

    // use it to do the calculation
    std::cout << "Calling function1..." << std::endl;

    function1();

    // close the library
    std::cout << "Closing library..." << std::endl;

    dlclose(handle);
}

