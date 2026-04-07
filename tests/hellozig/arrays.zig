const std = @import("std");
const print = std.debug.print;

pub fn main() !void {

    const a = [3]i32{ 1, 2, 3 };

    const b = [_]i32{ 4, 5, 6 };

    const c: [3]i32 = .{ 7, 8, 9 };

    var d: [3]i32 = undefined;
    d[0] = 10;
    d[1] = 11;
    d[2] = 12;

    print("len: {}\n", .{c.len});

    print("repeat: {any}\n", .{a ** 2});

    print("concat: {any}\n", .{a ++ b});

    for (d) |elem| {
        print("elem: {}\n", .{elem});
    }
}
