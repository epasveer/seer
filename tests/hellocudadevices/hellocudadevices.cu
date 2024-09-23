#include <stdio.h>

int main() {

    int nDevices;

    cudaError_t err = cudaGetDeviceCount(&nDevices);
    if (err != cudaSuccess) {
        printf("%s\n", cudaGetErrorString(err));
        return 1;
    }

    for (int i = 0; i < nDevices; i++) {
        cudaDeviceProp prop;
        cudaGetDeviceProperties(&prop, i);
        printf("Device Number: %d\n", i);
        printf("  Device name: %s\n", prop.name);
        printf("  Memory Clock Rate (KHz): %d\n", prop.memoryClockRate);
        printf("  Memory Bus Width (bits): %d\n", prop.memoryBusWidth);
        printf("  Peak Memory Bandwidth (GB/s): %f\n\n", 2.0*prop.memoryClockRate*(prop.memoryBusWidth/8)/1.0e6);
    }

    return 0;
}

