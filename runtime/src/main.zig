const std = @import("std");
const testing = std.testing;
pub const object = @import("object.zig");
const debug = std.debug;
pub const apply = @import("apply.zig");
const c = @import("c.zig");

fn main(best: u32, another: u32, final: u32, wow: u32, more: u32) u32 {
    _ = more;

    _ = best;
    _ = another;
    _ = final;
    _ = wow;
}
test "stuff" {
    debug.print("{}", .{c.ae_ref_count_is_thread_shared(1234)});
    // debug.print("{}", .{c.TESTING(1, 2)});
    // debug.print("{}", .{c.add(1, 2)});
    // debug.print("{}", .{@call(.always_inline, c.add, .{ 1, 2 })});
    // const bruh: *const Fn2 = &my_fn2;
    // _ = bruh;
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
