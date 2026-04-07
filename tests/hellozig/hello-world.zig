const std = @import("std");

pub fn main() !void {

    std.debug.print("Hello, World!\n", .{});

    const a = [3]i32{ 1, 2, 3 };
    const b = [_]i32{ 4, 5, 6 };
    const c: [3]i32 = .{ 7, 8, 9 };

    var d: [3]i32 = undefined;
    d[0] = 10;
    d[1] = 11;
    d[2] = 12;

    std.debug.print("len: {}\n", .{c.len});

    std.debug.print("repeat: {any}\n", .{a ** 2});

    std.debug.print("concat: {any}\n", .{a ++ b});

    for (d) |elem| {
        std.debug.print("elem: {}\n", .{elem});
    }
}

