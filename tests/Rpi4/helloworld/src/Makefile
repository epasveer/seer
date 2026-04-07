obj-m += helloworld.o

EXTRA_CFLAGS += -g -Wall -O0 -DDEBUG
CFLAGS += -g -O0

all:
	make -C $(KERNEL_SRC) M=$(PWD) modules

clean:
	make -C $(KERNEL_SRC) M=$(PWD) clean
