.PHONY: all
all: hello-world assignment integers

hello-world: hello-world.zig
	zig build-exe -O Debug hello-world.zig

assignment: assignment.zig
	zig build-exe -O Debug assignment.zig

integers: integers.zig
	zig build-exe -O Debug integers.zig

.PHONY: clean
clean:
	rm -f *.o hello-world assignment integers

