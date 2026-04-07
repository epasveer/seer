const std = @import("std");

pub fn main() !void {

    const c: bool = true;

    var v: bool = false;
    v = true;

    const inferred = true;

    var u: bool = undefined;
    u = true;

    _ = c;
    _ = inferred;
}
