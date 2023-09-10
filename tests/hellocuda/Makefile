.PHONY: all
all: hellocuda

hellocuda: hellocuda.cu
	/usr/local/cuda-12.2/bin/nvcc -g -G hellocuda.cu -o hellocuda

.PHONY: clean
clean:
	rm -f hellocuda hellocuda.o

