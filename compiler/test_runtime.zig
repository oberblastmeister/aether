const std = @import("std");
const debug = std.debug;

extern fn entry_point() void;

export fn print_u32(i: u32) void {
    debug.print("Hello World! {}\n", .{i});
}

export fn assert_u32(i: u32, j: u32) void {
    debug.assert(i == j);
}

export fn assert_u64(i: u32, j: u32) void {
    debug.assert(i == j);
}

pub fn main() void {
    entry_point();
}
