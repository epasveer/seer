const std = @import("std");
const print = std.debug.print;

pub fn main() !void {
    var x: i8 = 10;

    switch (x) {
        -1...1 => {
            x = -x;
        },
        10, 100 => print("x: {d}\n", .{x}),
        else => {},
    }

    var count: u8 = 1;
    while (count <= 15) : (count += 1) {

        const div3: u2 = @intFromBool(count % 3 == 0);

        const div5 = @intFromBool(count % 5 == 0);

        switch (div3 * 2 + div5) {
            0b10 => print("Fizz\n", .{}),
            0b01 => print("Buzz\n", .{}),
            0b11 => {
                print("Fizz", .{});
                print("Buzz\n", .{});
            },
            else => print("{d}\n", .{count}),
        }
    }

    x = switch (x) {
        -1...1 => -x,
        10, 100 => @divExact(x, 10),
        else => x,
    };
    print("x: {d}\n", .{x});
}
