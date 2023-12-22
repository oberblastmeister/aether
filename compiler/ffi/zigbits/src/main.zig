const std = @import("std");
const testing = std.testing;
const hash = std.hash;
const intern = @import("intern.zig");
const debug = std.debug;
const Lazy = @import("lazy.zig").Lazy;
const math = std.math;
const Thread = std.Thread;

fn init() intern.Interner {
    if (intern.Interner.init(std.heap.c_allocator)) |res| {
        return res;
    } else |err| {
        debug.panic("Failed to initialize interner: {}", .{err});
    }
}

var interner: Lazy(intern.Interner, init) = .{};

export fn zig_intern_bytestring(str: [*]u8, offset: usize, len: usize) u64 {
    if (interner.get().get_or_intern(
        std.heap.c_allocator,
        str[offset..len],
    )) |symbol| {
        return symbol.to_u64();
    } else |err| {
        debug.panic("failed to intern bytestring: {}", .{err});
    }
}

export fn zig_hash_bytestring_wyhash(str: [*]u8, offset: usize, len: usize) u64 {
    const slice: []u8 = str[offset..len];
    var hasher = hash.Wyhash.init(0);
    hash.autoHashStrat(&hasher, slice, .Deep);
    return hasher.final();
}

test "decls" {
    std.testing.refAllDecls(intern);
}
