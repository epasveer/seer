const std = @import("std");
const print = std.debug.print;

pub fn main() !void {

    const a: bool = true;
    var x: u16 = 0;

    if (a) {
        x += 1;
    } else {
        x += 2;
    }

    print("x = 1? {}\n", .{x == 1});

    x += if (x == 1) 1 else 2;
    print("x now is: {d}\n", .{x});
}
