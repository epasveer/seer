const std = @import("std");
const print = std.debug.print;

const a: f16 = 1.0;
const b: f32 = 100.0;
const c: f64 = 1_000.0;
const d: f128 = 10_000.0;

const e: comptime_float = 100_000.0;
const f = 1_000_000.0;

pub fn main() !void {
    print("float: {}\n", .{f});
}
