.PHONY: all
all: hello-world arrays assignment floats for ifelse integers pointers slices switch while

hello-world: hello-world.zig
	zig build-exe -O Debug hello-world.zig

arrays: arrays.zig
	zig build-exe -O Debug arrays.zig

assignment: assignment.zig
	zig build-exe -O Debug assignment.zig

floats: floats.zig
	zig build-exe -O Debug floats.zig

for: for.zig
	zig build-exe -O Debug for.zig

ifelse: ifelse.zig
	zig build-exe -O Debug ifelse.zig

index: index.zig
	zig build-exe -O Debug index.zig

integers: integers.zig
	zig build-exe -O Debug integers.zig

pointers: pointers.zig
	zig build-exe -O Debug pointers.zig

slices: slices.zig
	zig build-exe -O Debug slices.zig

switch: switch.zig
	zig build-exe -O Debug switch.zig

while: while.zig
	zig build-exe -O Debug while.zig

.PHONY: clean
clean:
	rm -f *.o hello-world arrays assignment floats for ifelse index integers pointers slices switch while

