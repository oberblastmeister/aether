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

    fn calculate_shard_amount() !usize {
        const cpu_count = if (Thread.getCpuCount()) |it| it else |_| 1;
        return try math.ceilPowerOfTwo(usize, cpu_count * 4);
    }

    pub fn init(allocator: Allocator) !Interner {
        const num_shards = try calculate_shard_amount();
        return try Interner.init_with(num_shards, allocator);
    }

    pub fn init_with(shard_amount: usize, allocator: Allocator) !Interner {
        const shift: u6 = @truncate(@typeInfo(usize).Int.bits - @ctz(shard_amount));
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

    pub fn resolve(self: *Self, symbol: Symbol) []const u8 {
        const shard = symbol.shard;
        const lock = &self.locks[shard];
        lock.lockShared();
        defer lock.unlockShared();
        return self.shards[shard].resolve(symbol.id);
    }

    pub fn get_or_intern(self: *Self, allocator: Allocator, string: []const u8) !Symbol {
        const hash = WyHash.hash(0, string);
        const shard = self.determine_shard(hash);
        const lock = &self.locks[shard];
        lock.lockShared();
        const res = self.shards[shard].get(hash, string);
        lock.unlockShared();
        const id = label: {
            if (res) |it| {
                break :label it;
            } else {
                // we need to use get_or_intern instead of intern after acquiring this lock
                // this is because another thread might have successfully interned the same string
                // between lock.unlockShared() and lock.lock()
                lock.lock();
                defer lock.unlock();
                break :label try self.shards[shard].get_or_intern(allocator, hash, string);
            }
        };
        return .{ .len = @intCast(string.len), .shard = shard, .id = id };
    }
};

const Id = u28;

pub const Symbol = packed struct {
    id: Id,
    shard: u16,
    len: u20,
};

const InternShard = struct {
    frozen: ArrayListUnmanaged([]const u8) = .{},
    buffer: ArrayListUnmanaged(u8) = .{},
    strings: ArrayListUnmanaged([]const u8) = .{},
    map: HashMapUnmanaged(Id, void, Context, 80) = .{},

    const Self = @This();

    pub const Context = struct {
        cached_hash: u64,
        string: []const u8,
        strings: [][]const u8,

        pub fn resolve_string(self: Context, id: Id) []const u8 {
            if (id == self.strings.len) {
                return self.string;
            }
            return self.strings[id];
        }

        pub fn hash(self: Context, id: Id) u64 {
            if (id == self.strings.len) {
                return self.cached_hash;
            }
            return WyHash.hash(0, self.strings[id]);
        }

        pub fn eql(self: Context, id1: Id, id2: Id) bool {
            return mem.eql(u8, self.resolve_string(id1), self.resolve_string(id2));
        }
    };

    pub const KeyContext = struct {
        context: Context,

        pub fn hash(self: KeyContext, string: []const u8) u64 {
            _ = string;

            return self.context.cached_hash;
        }

        pub fn eql(self: KeyContext, string: []const u8, id: Id) bool {
            return mem.eql(u8, string, self.context.strings[id]);
        }
    };

    pub fn context(self: *Self, hash: u64, string: []const u8) Context {
        return .{
            .cached_hash = hash,
            .string = string,
            .strings = self.strings.items,
        };
    }

    pub fn key_context(self: *Self, hash: u64, string: []const u8) KeyContext {
        return .{
            .context = self.context(hash, string),
        };
    }

    pub fn get(self: *Self, hash: u64, string: []const u8) ?Id {
        return self.map.getKeyAdapted(string, self.key_context(hash, string));
    }

    pub fn resolve(self: *Self, id: Id) []const u8 {
        return self.strings.items[id];
    }

    pub fn get_or_intern(self: *Self, allocator: Allocator, hash: u64, string: []const u8) !Id {
        const id: Id = @intCast(self.strings.items.len);
        const res = try self.map.getOrPutContext(
            allocator,
            id,
            self.context(hash, string),
        );
        if (res.found_existing) {
            // we know that it must be different from our id because our id is referring to a nonexistant entry in strings
            debug.assert(res.key_ptr.* != id);
            return res.key_ptr.*;
        } else {
            debug.assert(res.key_ptr.* == id);
            const new_string = try self.alloc(allocator, string);
            // this makes the id we just inserted valid because it now points to this new_string
            try self.strings.append(allocator, new_string);
            return id;
        }
    }

    pub fn intern(self: *Self, allocator: Allocator, hash: u64, string: []const u8) !Id {
        const id: Id = @intCast(self.strings.items.len);
        const new_string = try self.alloc(allocator, string);
        try self.strings.append(allocator, new_string);
        const res = try self.map.getOrPutContextAdapted(
            allocator,
            new_string,
            self.key_context(hash, string),
            self.context(hash, string),
        );
        debug.assert(!res.found_existing);
        res.key_ptr.* = id;
        return id;
    }

    fn alloc(self: *Self, allocator: Allocator, string: []const u8) ![]const u8 {
        const cap = self.buffer.capacity;
        if (cap < self.buffer.items.len + string.len) {
            const new_cap = try math.ceilPowerOfTwo(usize, @max(cap, string.len) + 1);
            var buffer = try ArrayListUnmanaged(u8).initCapacity(allocator, new_cap);
            mem.swap(@TypeOf(buffer), &buffer, &self.buffer);
            try self.frozen.append(allocator, @constCast(buffer.items));
        }
        const start = self.buffer.items.len;
        self.buffer.appendSliceAssumeCapacity(string);
        return self.buffer.items[start..][0..string.len];
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
    const s: []const u8 = "asdfasdf";
    const allocator = std.heap.c_allocator;
    var interner = try Interner.init(allocator);
    const sym = try interner.get_or_intern(allocator, s);
    for (0..10000) |_| {
        try testing.expectEqual(sym, try interner.get_or_intern(allocator, s));
        try testing.expectEqualSlices(u8, s, interner.resolve(sym));
    }
}
