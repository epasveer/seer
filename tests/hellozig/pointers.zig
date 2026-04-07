const std = @import("std");
const print = std.debug.print;

const Single = *bool;

const Many = [*]bool;

const Null = ?*bool;

pub fn main() !void {

    var v = false;
    const ptr: *bool = &v;
    print("pointer: {}\n", .{ptr});

    ptr.* = true;
    print("value: {}\n", .{ptr.*});

    const const_ptr: *bool = &v;
    const_ptr.* = false;

    const cf = false;
    const ct = true;
    var ptr_to_const: *const bool = &cf;
    ptr_to_const = &ct;
}
