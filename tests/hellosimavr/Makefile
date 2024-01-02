PROGNAME    = pipower

AVR_BAUD    ?= 19200
AVR_PORT    ?= /dev/ttyACM0
FUSE_EXT    ?= 0xff
FUSE_HIGH   ?= 0xdf
FUSE_LOW    ?= 0x62
PROGRAMMER  ?= -c arduino 

DEVICE      = attiny85
CLOCK       = 1000000
FUSES       = -U lfuse:w:$(FUSE_LOW):m -U hfuse:w:$(FUSE_HIGH):m -U efuse:w:$(FUSE_EXT):m 
PORT	    = -P $(AVR_PORT) -b $(AVR_BAUD)
AVRDUDE     = avrdude -v $(PORT) $(PROGRAMMER) -p $(DEVICE) $(AVR_EXTRA_ARGS)

CC	= avr-gcc
CFLAGS	+= -std=c99 -Wall $(DEBUG) $(OFLAG) -DF_CPU=$(CLOCK) -mmcu=$(DEVICE) --short-enums

OBJS += \
	pipower.o \
	button.o \
	input.o \
	millis.o

DEPS = $(OBJS:.o=.dep)

all:	$(PROGNAME).hex

%.o: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -c $< -o $@

%.dep: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -MM $< -o $@

%.pre: %.c
	$(CPP) $(CPPFLAGS) $(CFLAGS) -E $< -o $@

%.o: %.S
	$(CC) $(CPPFLAGS) $(CFLAGS) -x assembler-with-cpp -c $< -o $@

%.s: %.c
	$(CC) $(CPPFLAGS) $(CFLAGS) -S $< -o $@

.PHONY: all deps flash fuse make load clean

dep: $(DEPS)

flash:	all
	$(AVRDUDE) -U flash:w:$(PROGNAME).hex:i

fuse:
	$(AVRDUDE) $(FUSES)

make: flash fuse

clean:
	rm -f $(PROGNAME).hex $(PROGNAME).elf $(OBJS) $(DEPS)

$(PROGNAME).elf: $(OBJS)
	$(CC) $(CFLAGS) -o $(PROGNAME).elf $(OBJS)

$(PROGNAME).hex: $(PROGNAME).elf
	rm -f $(PROGNAME).hex
	avr-objcopy -j .text -j .data -O ihex $(PROGNAME).elf $(PROGNAME).hex

include $(DEPS)
