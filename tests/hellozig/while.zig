const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    var a: usize = 0;
    var b: usize = 0;
    var c: usize = 0;
    var d: usize = 0;

    while (a < 2) {
        print("a: {}\n", .{a});
        a += 1;
    }

    while (b < 2) : (b += 1) {
        print("b: {}\n", .{b});
    }

    while (c < 4) : ({
        c += 1;
        c += 1;
    }) {
        print("c: {}\n", .{c});
    }

    while (true) {
        break;
    }

    while (true) : (d += 1) {
        if (d < 2) {
            print("d: {}\n", .{d});
            continue;
        }

        break;
    }
}
