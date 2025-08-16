.PHONY: all
all: hellotictactoe

hellotictactoe: hellotictactoe.m
	clang hellotictactoe.m -g `gnustep-config --objc-flags` -std=c99 -lobjc -lgnustep-base -o hellotictactoe

.PHONY: clean
clean:
	rm -f hellotictactoe *.d

