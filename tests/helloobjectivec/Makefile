.PHONY: all
all: hellotictactoe hellodays

hellotictactoe: hellotictactoe.m
	clang hellotictactoe.m `gnustep-config --objc-flags` -O0 -std=c99 -lobjc -lgnustep-base -o hellotictactoe

hellodays: hellodays.m
	clang hellodays.m `gnustep-config --objc-flags` -O0 -std=c99 -lobjc -lgnustep-base -o hellodays

.PHONY: clean
clean:
	rm -f hellotictactoe hellodays *.d

