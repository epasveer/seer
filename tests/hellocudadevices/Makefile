.PHONY: all
all: hellocudadevices

hellocudadevices: hellocudadevices.cu
	/usr/local/cuda-11.4/bin/nvcc -g -G hellocudadevices.cu -o hellocudadevices

.PHONY: clean
clean:
	rm -f hellocudadevices hellocudadevices.o

