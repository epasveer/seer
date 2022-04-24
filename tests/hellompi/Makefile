EXECS=hellompi
MPICC=/usr/lib64/mpi/gcc/openmpi4/bin/mpicc

all: ${EXECS}

hellompi: hellompi.c
	${MPICC} -g -o hellompi hellompi.c

clean:
	rm ${EXECS}

