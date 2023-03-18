.PHONY: all
all: helloada

helloada: helloada.adb
	gnatmake --GCC=/usr/bin/gcc-7 -g -O0 helloada.adb

.PHONY: clean
clean:
	rm -f helloada helloada.o helloada.ali b~*

