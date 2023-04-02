.PHONY: all
all: helloadaexceptions

helloadaexceptions: helloadaexceptions.adb
	/usr/bin/gnatmake-9 -g --GNATBIND=/usr/bin/gnatbind-9 --GNATLINK=/usr/bin/gnatlink-9 helloadaexceptions

.PHONY: clean
clean:
	rm -f helloadaexceptions helloadaexceptions.o helloadaexceptions.ali b~*

