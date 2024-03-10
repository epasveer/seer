CPPFLAGS += -I..
CFLAGS += -DTIMER_BOOTWAIT=1000 -DTIMER_SHUTDOWN=1000 -DTIMER_POWEROFF=1000
OFLAG ?= -Og
DEBUG ?= -g

VPATH = ..

# Ensure malloc is available when debugging
OBJS += force_malloc.o

ifeq ($(TRACE), 1)
OBJS += simavr.o
endif

%.svg: %.dot
	dot -T svg -o $@ $<

include ../Makefile

.PHONY: simavr
simavr:
	simavr -m attiny85 -f 1000000 pipower.elf -t -g
