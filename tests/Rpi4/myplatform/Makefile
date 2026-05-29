obj-m += myplatform.o
EXTRA_CFLAGS += -g -Wall -O0 -DDEBUG
CFLAGS += -g -O0

all:
	$(MAKE) -C $(KERNEL_SRC) M=$(PWD) modules

modules_install:
	$(MAKE) -C $(KERNEL_SRC) M=$(PWD) modules_install

clean:
	$(MAKE) -C $(KERNEL_SRC) M=$(PWD) clean
