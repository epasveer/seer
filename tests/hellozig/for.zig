// _For loops_ can be used to iterate over sequences.

const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    var array = [_]u32{ 1, 2, 3 };

    // Here, we iterate over `array` by _value_, storing a copy of each element
    // in `elem`. Note that since `elem` is just a copy, we cannot use it to
    // modify `array`'s contents.
    for (array) |elem| {
        print("by val: {}\n", .{elem});
    }

    // To iterate by _reference_, we can loop over a slice of `array` and
    // prefix `elem` with a `*`. Here, `elem` is a pointer to an element in
    // `array`, which we can use to modify `array`'s contents.
    for (&array) |*elem| {
        elem.* += 100;
        print("by ref: {}\n", .{elem.*});
    }

    // Here, we iterate over multiple sequences. Note that both sequences
    // _must_ have the same length.
    for (array, &array) |val, *ref| {
        _ = val;
        _ = ref;
    }

    // You may also specify a _range_ with the `start..end` syntax. Note that
    // `end` may be omitted if another sequence is being iterated over as well;
    // the compiler will infer the range's size.
    for (0.., array) |i, elem| {
        print("{}: {}\n", .{ i, elem });
    }

    // To ignore the elements of a sequence, use `_`.
    for (array) |_| {}
}

