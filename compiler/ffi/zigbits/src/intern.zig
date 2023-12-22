const std = @import("std");
const RwLock = std.Thread.RwLock;
const Mutex = std.Thread.Mutex;
const math = std.math;
const HashMap = std.AutoHashMap;
const mem = std.mem;
const ArrayList = std.ArrayList;
const ArrayListUnmanaged = std.ArrayListUnmanaged;
const HashMapUnmanaged = std.HashMapUnmanaged;
const atomic = std.atomic;
const heap = std.heap;
const Allocator = std.mem.Allocator;
const WyHash = std.hash.Wyhash;
const Thread = std.Thread;
const debug = std.debug;
const meta = std.meta;

pub const Interner = struct {
    shift: u6,
    locks: []RwLock,
    shards: []InternShard,

    const Self = @This();

    pub fn init(allocator: Allocator) !Interner {
        const cpu_count = if (Thread.getCpuCount()) |it| it else |_| 1;
        const num_shards = try math.ceilPowerOfTwo(usize, cpu_count * 4);
        return try Interner.init_with(num_shards, allocator);
    }

    pub fn init_with(shard_amount: usize, allocator: Allocator) !Interner {
        // debug.print("shard amoutn: {}\n", .{shard_amount});
        const shift: u6 = @truncate(@typeInfo(usize).Int.bits - @ctz(shard_amount));
        // debug.print("shift: {}\n", .{shift});
        const shards = try allocator.alloc(InternShard, shard_amount);
        const locks = try allocator.alloc(RwLock, shard_amount);
        for (0..shard_amount) |i| {
            locks[i] = .{};
            shards[i] = .{};
        }
        return .{
            .shift = shift,
            .locks = locks,
            .shards = shards,
        };
    }

    pub fn determine_shard(self: *Self, hash: u64) u16 {
        return @truncate(hash >> self.shift);
    }

    pub fn get_or_intern(self: *Self, allocator: Allocator, string: []const u8) !Symbol {
        const hash = WyHash.hash(0, string);
        const shard = self.determine_shard(hash);
        const lock = &self.locks[shard];
        lock.lockShared();
        const id = label: {
            if (self.shards[shard].get(string, hash)) |it| {
                lock.unlockShared();
                break :label it;
            } else {
                lock.unlockShared();
                lock.lock();
                defer lock.unlock();
                break :label try self.shards[shard].get_or_intern(allocator, string, hash);
            }
        };
        return .{ .shard = shard, .id = id };
    }
};

pub const Symbol = struct {
    shard: u32,
    id: u32,

    pub inline fn to_u64(self: Symbol) u64 {
        return @as(u64, self.shard) << 32 | self.id;
    }
};

const InternShard = struct {
    buffer: ArrayListUnmanaged(u8) = .{},
    ends: ArrayListUnmanaged(u32) = .{},
    map: HashMapUnmanaged(u32, void, Context, 80) = .{},
    next_id: u32 = 0,

    const Self = @This();

    pub const Context = struct {
        buffer: []u8,
        ends: []u32,

        pub fn resolve_string(self: *const Context, symbol: u32) []u8 {
            const from = if (symbol == 0) 0 else self.ends[symbol - 1];
            const to = self.ends[symbol];
            return self.buffer[from..to];
        }

        pub fn hash(self: Context, symbol1: u32) u64 {
            return WyHash.hash(0, self.resolve_string(symbol1));
        }

        pub fn eql(self: Context, symbol1: u32, symbol2: u32) bool {
            return mem.eql(u8, self.resolve_string(symbol1), self.resolve_string(symbol2));
        }
    };

    pub const KeyContext = struct {
        cached_hash: u64,
        cxt: Context,

        pub fn hash(self: KeyContext, string: []const u8) u64 {
            _ = string;

            return self.cached_hash;
        }

        pub fn eql(self: KeyContext, string: []const u8, id: u32) bool {
            return mem.eql(u8, string, self.cxt.resolve_string(id));
        }
    };

    pub fn context(self: *Self) Context {
        return .{
            .buffer = self.buffer.items,
            .ends = self.ends.items,
        };
    }

    pub fn key_context(self: *Self, hash: u64) KeyContext {
        return .{
            .cached_hash = hash,
            .cxt = self.context(),
        };
    }

    pub fn get(self: *Self, string: []const u8, hash: u64) ?u32 {
        return self.map.getKeyAdapted(string, self.key_context(hash));
    }

    pub fn get_or_intern(self: *Self, allocator: Allocator, string: []const u8, hash: u64) !u32 {
        const id = self.next_id;
        self.next_id += 1;
        const res = try self.map.getOrPutContextAdapted(
            allocator,
            string,
            self.key_context(hash),
            self.context(),
        );
        if (res.found_existing) {
            return res.key_ptr.*;
        } else {
            res.key_ptr.* = id;
            try self.buffer.appendSlice(allocator, string);
            const to = self.buffer.items.len;
            try self.ends.append(allocator, @truncate(to));
            return id;
        }
    }
};

test "basic" {
    const testing = std.testing;
    const allocator = std.heap.c_allocator;
    var interner = try Interner.init(allocator);
    const sym1 = try interner.get_or_intern(allocator, "first");
    const sym2 = try interner.get_or_intern(allocator, "second");
    try testing.expect(!meta.eql(sym1, sym2));
    const sym3 = try interner.get_or_intern(allocator, "first");
    try testing.expectEqual(sym3, sym1);
}

test "repeat" {
    const testing = std.testing;
    const s = "asdfasdf";
    const allocator = std.heap.c_allocator;
    var interner = try Interner.init(allocator);
    const sym = try interner.get_or_intern(allocator, s);
    for (0..10000) |_| {
        try testing.expectEqual(sym, try interner.get_or_intern(allocator, s));
    }
}
