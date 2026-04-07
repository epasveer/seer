const std = @import("std");
const print = std.debug.print;

const Slice = []bool;

pub fn main() !void {

    var array = [5]i32{ 1, 2, 3, 4, 5 };
    const end: usize = 4;
    const slice = array[1..end];

    print("len: {}\n", .{slice.len});
    print("first: {}\n", .{slice[0]});
    for (slice) |elem| {
        print("elem: {}\n", .{elem});
    }

    const ptr: *[3]i32 = array[1..4];

    print("len: {}\n", .{ptr.len});
    print("first: {}\n", .{ptr[0]});
    for (ptr) |elem| {
        print("elem: {}\n", .{elem});
    }
}
