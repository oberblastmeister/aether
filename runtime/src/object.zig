// runtime object api

const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const Type = std.builtin.Type;
const atomic = std.atomic;
const debug = std.debug;
const Allocator = std.mem.Allocator;
const testing = std.testing;

pub export fn ae_panic(s: [*:0]u8) void {
    debug.panic("{s}", .{s});
}

pub const Tag = enum(u8) {
    Fun,
    Pap,
};

const alignment = @alignOf(usize);

comptime {
    debug.assert(@sizeOf(Object) == @sizeOf(Box));
    debug.assert(@sizeOf(Box) == @sizeOf(*anyopaque));
}

test {
    try testing.expectEqual(16, @sizeOf(Closure));
}

pub const RefCount = atomic.Value(u32);

pub fn assert_type_object(comptime O: type) void {
    debug.assert(meta.fields(O)[0].type == Object);
    debug.assert(@alignOf(O) == @alignOf(Box));
    debug.assert(@sizeOf(O) % @sizeOf(Box) == 0);
}

pub inline fn ref_count_is_thread_shared(rc: u32) bool {
    return @as(i32, @bitCast(rc)) < 0;
}

pub inline fn ref_count_is_unique_or_thread_shared(rc: u32) bool {
    return @as(i32, @bitCast(rc)) <= 0;
}

//--------------------------------------------------------------------------------------
//   Checked reference counts.

//   positive:
//     0                         : unique reference
//     0x00000001 - 0x7FFFFFFF   : reference count (in a single thread)   (~2.1e9 counts)
//   negative:
//     0x80000000                : sticky: single-threaded stricky reference count (RC_STUCK)
//     0x80000001 - 0xA0000000   : sticky: neither increment, nor decrement
//     0xA0000001 - 0xFFFFFFFF   : thread-shared reference counts with atomic increment/decrement. (~1.6e9 counts)
//     0xFFFFFFFF                : RC_SHARED_UNIQUE (-1)

//   0 <= refcount <= MAX_INT32
//     regular reference counts where use 0 for a unique reference. So a reference count of 10 means there
//     are 11 reference (from the current thread only).
//     If it is dup'd beyond MAX_INT32 it'll overflow automatically into the sticky range (as a negative value)

//   MAX_INT32 < refcount <= MAX_UINT32
//     Thread-shared and sticky reference counts. These use atomic increment/decrement operations.

//   MAX_INT32 + 1 == RC_STUCK
//     This is used for single threaded refcounts that overflow. (This is sticky and the object will never be freed)
//     The thread-shared refcounts will never get there.

//   RC_STUCK < refcount <= RC_STICKY
//     The sticky range. An object in this range will never be freed anymore.
//     Since we first read the reference count non-atomically we need a range
//     for stickiness. Once `refcount <= RC_STICKY_DROP` it will never drop anymore
//     (increment the refcount), and once refcount <= RC_STICKY it will never dup/drop anymore.
//     We assume that the relaxed reads of the reference counts catch up to the atomic
//     value within the sticky range (which has a range of ~0.5e9 counts).

//   Atomic memory ordering:
//   - Increments can be relaxed as there is no dependency on order, the owner
//     could access fields just as well before or after incrementing.
//   - Decrements must use release order though: after decrementing the owner should
//     no longer read/write to the object so no reads/writes are allowed to be reordered
//     to occur after the decrement.
//   - If the decrement causes the object to be freed, we also need to acquire: any reads/writes
//     that occur after the final decrement should similarly not be reordered just before it.
//   - see also: https://devblogs.microsoft.com/oldnewthing/20210409-00/?p=105065
// --------------------------------------------------------------------------------------
pub const RC_STUCK: u32 = 0x80000000;
pub const RC_STICKY: u32 = 0xA0000000;
pub const RC_SHARED_UNIQUE: u32 = 0xFFFFFFFF;

pub const Object = extern struct {
    fields: u8,
    _field_index: u8,
    other: u8,
    tag: Tag,
    ref_count: RefCount,

    const Self = @This();

    pub fn free(self: *Object) void {
        _ = self;
    }

    pub fn dup_atomic(self: *Object) u32 {
        return self.ref_count.fetchSub(1, .Monotonic);
    }

    pub fn drop_atomic(self: *Object) u32 {
        return self.ref_count.fetchAdd(1, .AcqRel);
    }

    pub fn dup_cold(self: *Object, rc: u32) void {
        debug.assert(ref_count_is_thread_shared(rc));
        if (rc > RC_STICKY) {
            _ = self.dup_atomic();
        }
    }

    pub inline fn dup(self: *Object) void {
        const rc = self.ref_count.load(.Monotonic);
        if (ref_count_is_thread_shared(rc)) {
            self.dup_cold(rc);
        } else {
            self.ref_count.store(rc + 1, .Monotonic);
        }
    }

    pub fn drop_cold(self: *Object, rc: u32) void {
        debug.assert(ref_count_is_unique_or_thread_shared(rc));
        if (rc == 0) {
            self.free();
        } else if (rc <= RC_STICKY) {
            // includes RC_STUCK
            // sticky
        } else {
            if (self.drop_atomic() == RC_SHARED_UNIQUE) {
                self.free();
            }
        }
    }

    pub inline fn is_unique(self: *Object) bool {
        return self.ref_count.load(.Monotonic) == 0;
    }

    pub inline fn drop(self: *Object) void {
        const rc = self.ref_count.load(.Monotonic);
        if (ref_count_is_unique_or_thread_shared(rc)) {
            self.drop_cold(rc);
        } else {
            self.ref_count.store(rc - 1, .Monotonic);
        }
    }

    pub inline fn box(self: *Object) Box {
        return .{ .box = @ptrCast(self) };
    }

    pub inline fn castFrom(o: anytype) *Object {
        return @alignCast(@ptrCast(o));
    }

    pub inline fn cast(self: *Object, comptime O: type) *O {
        comptime {
            assert_type_object(O);
        }
        return @alignCast(@ptrCast(self));
    }

    pub fn alloc(comptime O: type, fields: u8) *O {
        const allocator = std.heap.c_allocator;
        comptime {
            assert_type_object(O);
        }
        const p = label: {
            if (allocator.alloc(Box, @sizeOf(O) / @sizeOf(Box) + fields)) |p| {
                break :label p;
            } else |e| {
                debug.panic("failed to allocate memory: {}", .{e});
            }
        };
        const h: *Object = @ptrCast(p);
        h.fields = fields;
        h._field_index = 0;
        h.tag = O.tag;
        h.ref_count = RefCount.init(1);
        return @ptrCast(p);
    }

    pub export fn allocClosure(env_size: u8, arity: u8) *Closure {
        const h = Object.alloc(Closure, env_size);
        h.header.other = arity;
        return h;
    }

    pub fn alloc_pap(closure: *Closure, arity: u8, fixed: u8) *Pap {
        const o = Object.alloc(Pap, fixed);
        o.closure = closure;
        o.header.other = arity;
        return o;
    }
};

test {
    const closure = Object.alloc(Closure, 0);
    try testing.expectEqual(closure.header.tag, Closure.tag);
}

pub const Closure = extern struct {
    header: Object,
    code: *anyopaque,

    pub const tag: Tag = .Fun;

    pub inline fn arity(self: *Closure) u8 {
        return self.header.other;
    }

    pub inline fn env(self: *Closure) []Box {
        return (self + @sizeOf(@This()))[0..self.header.fields];
    }
};

pub const Pap = extern struct {
    header: Object,
    closure: *Closure,

    pub const tag: Tag = .Pap;

    pub inline fn arity(self: *Pap) u8 {
        return self.header.other;
    }

    pub inline fn fixed(self: *Pap) []Box {
        const p: [*]Box = @ptrCast(@as([*]Pap, @ptrCast(self)) + 1);
        return p[0..(self.header.fields + self.arity())];
    }
};

pub const Value = usize;

pub const Box = extern struct {
    box: *anyopaque,

    pub export fn box_fun(self: Box) bool {
        _ = self;
        return true;
    }

    pub inline fn is_value(self: Box) bool {
        return @intFromPtr(self.box) & 1 == 1;
    }

    pub inline fn as_value(self: Box) bool {
        debug.assert(self.is_value());
        return @intFromPtr(self.box) >> 1;
    }

    pub inline fn is_object(self: Box) bool {
        return @intFromPtr(self.box) & 1 == 0;
    }

    pub inline fn as_object(self: Box) *Object {
        debug.assert(self.is_object());
        return @ptrCast(@alignCast(self.box));
    }

    pub inline fn drop(self: Box) void {
        if (self.is_object()) {
            self.as_object().drop();
        }
    }

    pub inline fn dup(self: Box) void {
        if (self.is_object()) {
            self.as_object().dup();
        }
    }
};
