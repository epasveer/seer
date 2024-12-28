#include "hip/hip_runtime.h"
#include <cassert>
#include <cstdio>

__global__
void do_an_addition (int a, int b, int *out) {
    *out = a + b;
}

int main () {
    int *result_ptr, result;

    /* Allocate memory for the device to write the result to.  */
    hipError_t error_code = hipMalloc (&result_ptr, sizeof (int));

    printf("HIP Error %d %s: %s.\n", error_code, hipGetErrorName(error_code), hipGetErrorString(error_code));
    assert (error_code == hipSuccess);

    /* Run `do_an_addition` on one workgroup containing one work item.  */
    do_an_addition<<<dim3(1), dim3(1), 0, 0>>> (1, 2, result_ptr);

    /* Copy result from device to host.  Note that this acts as a synchronization
       point, waiting for the kernel dispatch to complete.  */
    error_code = hipMemcpyDtoH (&result, result_ptr, sizeof (int));

    printf("HIP Error %d %s: %s.\n", error_code, hipGetErrorName(error_code), hipGetErrorString(error_code));
    assert (error_code == hipSuccess);

    printf ("result is %d\n", result);
    assert (result == 3);

    return 0;
}

