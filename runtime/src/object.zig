const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const Type = std.builtin.Type;
const atomic = std.atomic;
const debug = std.debug;
const Allocator = std.mem.Allocator;
const testing = std.testing;

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

pub const Object = extern struct {
    fields: u8,
    _field_index: u8,
    other: u8,
    tag: Tag,
    ref_count: RefCount,

    const Self = @This();

    pub inline fn inc(self: *Object) void {
        _ = self;
    }

    pub inline fn dec(self: *Object) void {
        _ = self;
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

    pub fn allocClosure(env_size: u8, arity: u8) *Closure {
        const h = Object.alloc(Closure, env_size);
        h.other = arity;
        return h;
    }

    pub fn allocPap(closure: *Closure, arity: u8, fixed: u8) *Pap {
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

// pub const Pap = extern struct {
//     header: Header,
//     fun: *Closure,
//     arity: u32,
//     fixed: u32,

//     pub inline fn args(self: *Pap) []Box {
//         return (self + @sizeOf(@This()))[0..self.header.fields];
//     }
// };

pub const Value = usize;

pub const Box = extern struct {
    box: *anyopaque,

    pub inline fn deinit(self: Box, allocator: Allocator) void {
        allocator.free(self.box);
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

    // pub inline fn from_object(self: Box)
    pub inline fn as_object(self: Box) *Object {
        debug.assert(self.is_object());
        return @ptrCast(@alignCast(self.box));
    }

    pub inline fn dec(self: Box) void {
        _ = self;
    }

    pub inline fn inc(self: Box) void {
        _ = self;
    }
};
