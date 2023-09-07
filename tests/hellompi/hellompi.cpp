#include <mpi.h>
#include <sys/types.h>
#include <unistd.h>
#include <iostream>

int main(int argc, char** argv) {

    // Initialize the MPI environment
    MPI_Init(NULL, NULL);

    // Get the number of processes
    int world_size;
    MPI_Comm_size(MPI_COMM_WORLD, &world_size);

    // Get the rank of the process
    int world_rank;
    MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);

    // Get the name of the processor
    char processor_name[MPI_MAX_PROCESSOR_NAME];
    int name_len;
    MPI_Get_processor_name(processor_name, &name_len);

    // Print off a hello world message
    std::cout << "Hello world from processor " << processor_name << " rank " << world_rank << " size " << world_size << std::endl;

    std::cout << "Waiting for debugger to be attached, PID: " << getpid() << std::endl;

    bool attached = false;

    while (!attached) sleep(1);

    // Finalize the MPI environment.
    MPI_Finalize();
}

