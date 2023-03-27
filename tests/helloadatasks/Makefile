.PHONY: all
all: helloadatasks

helloadatasks: helloadatasks.adb
	/usr/bin/gnatmake-9 -g --GNATBIND=/usr/bin/gnatbind-9 --GNATLINK=/usr/bin/gnatlink-9 helloadatasks

.PHONY: clean
clean:
	rm -f helloadatasks helloadatasks.o helloadatasks.ali b~*

# /usr/bin/gnatmake-9 --GCC=/usr/bin/gcc-9 -g helloadatasks
