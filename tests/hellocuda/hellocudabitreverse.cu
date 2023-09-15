#include <stdio.h>
#include <stdlib.h>

// https://docs.nvidia.com/cuda/cuda-gdb/index.html#walk-through-examples

// Simple 8-bit bit reversal Compute test

#define N 256

__global__ void bitreverse (void *data) {

    unsigned int *idata = (unsigned int*)data;
    extern __shared__ int array[];

    array[threadIdx.x] = idata[threadIdx.x];

    array[threadIdx.x] = ((0xf0f0f0f0 & array[threadIdx.x]) >> 4) | ((0x0f0f0f0f & array[threadIdx.x]) << 4);
    array[threadIdx.x] = ((0xcccccccc & array[threadIdx.x]) >> 2) | ((0x33333333 & array[threadIdx.x]) << 2);
    array[threadIdx.x] = ((0xaaaaaaaa & array[threadIdx.x]) >> 1) | ((0x55555555 & array[threadIdx.x]) << 1);

    idata[threadIdx.x] = array[threadIdx.x];
}

int main(void) {

    void* d = NULL; 
    int i;
    unsigned int idata[N], odata[N];

    for (i = 0; i < N; i++) {
        idata[i] = (unsigned int)i;
    }

    cudaMalloc((void**)&d, sizeof(int)*N);
    cudaMemcpy(d, idata, sizeof(int)*N, cudaMemcpyHostToDevice);

    bitreverse<<<1, N, N*sizeof(int)>>>(d);

    cudaMemcpy(odata, d, sizeof(int)*N, cudaMemcpyDeviceToHost);

    for (i = 0; i < N; i++) {
        printf("%u -> %u\n", idata[i], odata[i]);
    }

    cudaFree((void*)d);

    return 0;
}

