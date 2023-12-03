const std = @import("std");
const testing = std.testing;
const object = @import("object.zig");
const debug = std.debug;
const apply = @import("apply.zig");
const c = @import("c.zig");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

const Fn2 = fn (f: *object.Closure, arg0: object.Box, arg1: object.Box) void;

fn my_fn2(f: *object.Closure, arg0: object.Box, arg1: object.Box) void {
    _ = f;
    _ = arg0;
    _ = arg0;

    _ = arg1;
    const res = &apply.apply_boxed_n;
    _ = res(undefined, undefined);

    return;
}

test "stuff" {
    debug.print("{}", .{c.ADD(1, 2)});
    debug.print("{}", .{c.add(1, 2)});
    debug.print("{}", .{@call(.always_inline, c.add, .{ 1, 2 })});
    const bruh: *const Fn2 = &my_fn2;
    _ = bruh;
}

test "basic add functionality" {
    try testing.expect(add(3, 7) == 10);
}

// fn my_simple(o: object.Object) object.Object {
//     return o;
// }

// test {
//     const f = &my_simple;
//     const g: *object.MakeFn(2) = f;
//     debug.print("{a}", .{g});
//     debug.assert(1 == 2);
// }
