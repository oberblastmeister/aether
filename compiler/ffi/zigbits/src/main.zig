const std = @import("std");
const testing = std.testing;
const hash = std.hash;
const intern = @import("intern.zig");
const debug = std.debug;
const Lazy = @import("lazy.zig").Lazy;
const math = std.math;
const Thread = std.Thread;
const mem = std.mem;

fn init() intern.Interner {
    if (intern.Interner.init(std.heap.c_allocator)) |res| {
        return res;
    } else |err| {
        debug.panic("Failed to initialize interner: {}", .{err});
    }
}

var interner: Lazy(intern.Interner, init) = .{};

export fn zig_intern_resolve(symbol: u64) [*]const u8 {
    return interner.get().resolve(@bitCast(symbol)).ptr;
}

export fn zig_intern_compare(symbol1: u64, symbol2: u64) i8 {
    const i = interner.get();
    const s1 = i.resolve(@bitCast(symbol1));
    const s2 = i.resolve(@bitCast(symbol2));
    return switch (mem.order(u8, s1, s2)) {
        .lt => -1,
        .eq => 0,
        .gt => 1,
    };
}

export fn zig_intern_bytestring(str: [*]u8, offset: usize, len: usize) u64 {
    if (interner.get().get_or_intern(
        std.heap.c_allocator,
        str[offset .. offset + len],
    )) |symbol| {
        return @bitCast(symbol);
    } else |err| {
        debug.panic("failed to intern bytestring: {}", .{err});
    }
}

export fn zig_hash_bytestring_wyhash(str: [*]u8, offset: usize, len: usize) u64 {
    const slice: []u8 = str[offset .. offset + len];
    var hasher = hash.Wyhash.init(0);
    hash.autoHashStrat(&hasher, slice, .Deep);
    return hasher.final();
}

test "decls" {
    std.testing.refAllDecls(intern);
}
