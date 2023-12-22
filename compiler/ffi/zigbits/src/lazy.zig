const std = @import("std");

pub fn Lazy(comptime T: type, comptime f: fn () T) type {
    return struct {
        const Self = @This();

        var value: T = undefined;
        var once = std.once(init);

        fn init() void {
            value = f();
        }

        pub fn get(self: Self) *T {
            _ = self;

            once.call();
            return &value;
        }
    };
}
