const std = @import("std");
const testing = std.testing;
const object = @import("object.zig");
const debug = std.debug;
const apply = @import("apply.zig");

export fn add(a: i32, b: i32) i32 {
    return a + b;
}

const Fn2 = fn (f: *object.Closure, arg0: object.Box, arg1: object.Box) void;

fn my_fn2(f: *object.Closure, arg0: object.Box, arg1: object.Box) void {
    _ = f;
    _ = arg0;
    _ = arg1;
    return;
}

test "stuff" {
    const bruh: *const Fn2 = &my_fn2;
    _ = bruh;

    testing.refAllDecls(@This());
    testing.refAllDecls(object);
    testing.refAllDecls(apply);
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
