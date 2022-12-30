.PHONY: all
all: hellosymbolfile

hellosymbolfile: hellosymbolfile.cpp function1.cpp
	g++ -g -o hellosymbolfile hellosymbolfile.cpp function1.cpp
	objcopy --only-keep-debug hellosymbolfile hellosymbolfile.dbg
	strip --strip-debug --strip-unneeded hellosymbolfile

.PHONY: clean
clean:
	rm -f hellosymbolfile hellosymbolfile.dbg hellosymbolfile.o function1.o

