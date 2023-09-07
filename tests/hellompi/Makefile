EXECS=hellompi
#MPICC=/usr/lib64/mpi/gcc/openmpi4/bin/mpicc
MPI=/usr/lib64/mpi/gcc/mpich

all: ${EXECS}

hellompi: hellompi.cpp
	g++ -g -I${MPI}/include -L${MPI}/lib64 -lmpi -o hellompi hellompi.cpp

clean:
	rm ${EXECS}

