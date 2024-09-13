.PHONY: all
all: hellocudasimple hellocudabitreverse

hellocudasimple: hellocudasimple.cu
	/usr/local/cuda/bin/nvcc -g -G hellocudasimple.cu -o hellocudasimple

hellocudabitreverse: hellocudabitreverse.cu
	/usr/local/cuda/bin/nvcc -g -G hellocudabitreverse.cu -o hellocudabitreverse

.PHONY: clean
clean:
	rm -f hellocudasimple hellocudasimple.o hellocudabitreverse hellocudabitreverse.o

