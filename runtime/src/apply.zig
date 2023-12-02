// This file was generated! Do not edit!
const std = @import("std");
const debug = std.debug;
const object = @import("object.zig");
const Object = object.Object;
const Allocator = std.mem.Allocator;
const Closure = object.Closure;
const Pap = object.Pap;
const Box = object.Box;
pub const FnBoxed1 = fn (f: *Closure, arg0: Box) Box;
pub const FnBoxed2 = fn (f: *Closure, arg0: Box, arg1: Box) Box;
pub const FnBoxed3 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box) Box;
pub const FnBoxed4 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box) Box;
pub const FnBoxed5 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box) Box;
pub const FnBoxed6 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box) Box;
pub const FnBoxed7 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box) Box;
pub const FnBoxed8 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box) Box;
pub const FnBoxed9 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box) Box;
pub const FnBoxed10 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box) Box;
pub const FnBoxed11 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box) Box;
pub const FnBoxed12 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box) Box;
pub const FnBoxed13 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box) Box;
pub const FnBoxed14 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box) Box;
pub const FnBoxed15 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box) Box;
pub const FnBoxed16 = fn (f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box, arg15: Box) Box;
pub const FnBoxedN = fn (f: *Closure, args: [*]Box) Box;
pub inline fn call_closure_boxed_1(f: *Closure, arg0: Box) Box {
    const code: *FnBoxed1 = @ptrCast(f.code);
    return code(f, arg0);
}
pub inline fn call_closure_boxed_2(f: *Closure, arg0: Box, arg1: Box) Box {
    const code: *FnBoxed2 = @ptrCast(f.code);
    return code(f, arg0, arg1);
}
pub inline fn call_closure_boxed_3(f: *Closure, arg0: Box, arg1: Box, arg2: Box) Box {
    const code: *FnBoxed3 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2);
}
pub inline fn call_closure_boxed_4(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box) Box {
    const code: *FnBoxed4 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3);
}
pub inline fn call_closure_boxed_5(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box) Box {
    const code: *FnBoxed5 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4);
}
pub inline fn call_closure_boxed_6(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box) Box {
    const code: *FnBoxed6 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5);
}
pub inline fn call_closure_boxed_7(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box) Box {
    const code: *FnBoxed7 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
}
pub inline fn call_closure_boxed_8(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box) Box {
    const code: *FnBoxed8 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
}
pub inline fn call_closure_boxed_9(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box) Box {
    const code: *FnBoxed9 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
}
pub inline fn call_closure_boxed_10(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box) Box {
    const code: *FnBoxed10 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
}
pub inline fn call_closure_boxed_11(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box) Box {
    const code: *FnBoxed11 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
}
pub inline fn call_closure_boxed_12(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box) Box {
    const code: *FnBoxed12 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
}
pub inline fn call_closure_boxed_13(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box) Box {
    const code: *FnBoxed13 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
}
pub inline fn call_closure_boxed_14(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box) Box {
    const code: *FnBoxed14 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
}
pub inline fn call_closure_boxed_15(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box) Box {
    const code: *FnBoxed15 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
}
pub inline fn call_closure_boxed_16(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box, arg15: Box) Box {
    const code: *FnBoxed16 = @ptrCast(f.code);
    return code(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
}
pub fn apply_pap_boxed_1(pap: *Pap, arg0: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 1) {
        switch (arity) {
            1 => {
                return call_closure_boxed_1(closure, arg0);
            },
            2 => {
                return call_closure_boxed_2(closure, fixed[0], arg0);
            },
            3 => {
                return call_closure_boxed_3(closure, fixed[0], fixed[1], arg0);
            },
            4 => {
                return call_closure_boxed_4(closure, fixed[0], fixed[1], fixed[2], arg0);
            },
            5 => {
                return call_closure_boxed_5(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0);
            },
            6 => {
                return call_closure_boxed_6(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], arg0);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], arg0);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], arg0);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], fixed[13], arg0);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], fixed[13], fixed[14], arg0);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 1) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 1));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_2(pap: *Pap, arg0: Box, arg1: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 2) {
        switch (arity) {
            2 => {
                return call_closure_boxed_2(closure, arg0, arg1);
            },
            3 => {
                return call_closure_boxed_3(closure, fixed[0], arg0, arg1);
            },
            4 => {
                return call_closure_boxed_4(closure, fixed[0], fixed[1], arg0, arg1);
            },
            5 => {
                return call_closure_boxed_5(closure, fixed[0], fixed[1], fixed[2], arg0, arg1);
            },
            6 => {
                return call_closure_boxed_6(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0, arg1);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], arg0, arg1);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], arg0, arg1);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], arg0, arg1);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], fixed[13], arg0, arg1);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 2) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 2));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_3(pap: *Pap, arg0: Box, arg1: Box, arg2: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 3) {
        switch (arity) {
            3 => {
                return call_closure_boxed_3(closure, arg0, arg1, arg2);
            },
            4 => {
                return call_closure_boxed_4(closure, fixed[0], arg0, arg1, arg2);
            },
            5 => {
                return call_closure_boxed_5(closure, fixed[0], fixed[1], arg0, arg1, arg2);
            },
            6 => {
                return call_closure_boxed_6(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1, arg2);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0, arg1, arg2);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], arg0, arg1, arg2);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], arg0, arg1, arg2);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], fixed[12], arg0, arg1, arg2);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 3) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 3));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_4(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 4) {
        switch (arity) {
            4 => {
                return call_closure_boxed_4(closure, arg0, arg1, arg2, arg3);
            },
            5 => {
                return call_closure_boxed_5(closure, fixed[0], arg0, arg1, arg2, arg3);
            },
            6 => {
                return call_closure_boxed_6(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2, arg3);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1, arg2, arg3);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0, arg1, arg2, arg3);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], arg0, arg1, arg2, arg3);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], fixed[11], arg0, arg1, arg2, arg3);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 4) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 4));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_5(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 5) {
        switch (arity) {
            5 => {
                return call_closure_boxed_5(closure, arg0, arg1, arg2, arg3, arg4);
            },
            6 => {
                return call_closure_boxed_6(closure, fixed[0], arg0, arg1, arg2, arg3, arg4);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3, arg4);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2, arg3, arg4);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1, arg2, arg3, arg4);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0, arg1, arg2, arg3, arg4);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], fixed[10], arg0, arg1, arg2, arg3, arg4);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 5) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 5));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_6(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 6) {
        switch (arity) {
            6 => {
                return call_closure_boxed_6(closure, arg0, arg1, arg2, arg3, arg4, arg5);
            },
            7 => {
                return call_closure_boxed_7(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], fixed[9], arg0, arg1, arg2, arg3, arg4, arg5);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 6) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 6));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_7(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 7) {
        switch (arity) {
            7 => {
                return call_closure_boxed_7(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            8 => {
                return call_closure_boxed_8(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], fixed[8], arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 7) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 7));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_8(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 8) {
        switch (arity) {
            8 => {
                return call_closure_boxed_8(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            9 => {
                return call_closure_boxed_9(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], fixed[7], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 8) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 8));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_9(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 9) {
        switch (arity) {
            9 => {
                return call_closure_boxed_9(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            10 => {
                return call_closure_boxed_10(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], fixed[6], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 9) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 9));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_10(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 10) {
        switch (arity) {
            10 => {
                return call_closure_boxed_10(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            11 => {
                return call_closure_boxed_11(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], fixed[5], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 10) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 10));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_11(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 11) {
        switch (arity) {
            11 => {
                return call_closure_boxed_11(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            12 => {
                return call_closure_boxed_12(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], fixed[4], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 11) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 11));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_12(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 12) {
        switch (arity) {
            12 => {
                return call_closure_boxed_12(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            13 => {
                return call_closure_boxed_13(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], fixed[3], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 12) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        args[fixed.len + 11] = arg11;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 12));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        newPap.fixed()[fixed.len + 11] = arg11;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_13(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 13) {
        switch (arity) {
            13 => {
                return call_closure_boxed_13(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            14 => {
                return call_closure_boxed_14(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], fixed[2], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 13) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        args[fixed.len + 11] = arg11;
        args[fixed.len + 12] = arg12;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 13));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        newPap.fixed()[fixed.len + 11] = arg11;
        newPap.fixed()[fixed.len + 12] = arg12;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_14(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 14) {
        switch (arity) {
            14 => {
                return call_closure_boxed_14(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            15 => {
                return call_closure_boxed_15(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], fixed[1], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 14) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        args[fixed.len + 11] = arg11;
        args[fixed.len + 12] = arg12;
        args[fixed.len + 13] = arg13;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 14));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        newPap.fixed()[fixed.len + 11] = arg11;
        newPap.fixed()[fixed.len + 12] = arg12;
        newPap.fixed()[fixed.len + 13] = arg13;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_15(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 15) {
        switch (arity) {
            15 => {
                return call_closure_boxed_15(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            16 => {
                return call_closure_boxed_16(closure, fixed[0], arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 15) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        args[fixed.len + 11] = arg11;
        args[fixed.len + 12] = arg12;
        args[fixed.len + 13] = arg13;
        args[fixed.len + 14] = arg14;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 15));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        newPap.fixed()[fixed.len + 11] = arg11;
        newPap.fixed()[fixed.len + 12] = arg12;
        newPap.fixed()[fixed.len + 13] = arg13;
        newPap.fixed()[fixed.len + 14] = arg14;
        return Object.castFrom(newPap).box();
    }
}
pub fn apply_pap_boxed_16(pap: *Pap, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box, arg15: Box) Box {
    const closure = pap.closure;
    const arity = pap.arity();
    const fixed = pap.fixed();
    if (arity == fixed.len + 16) {
        switch (arity) {
            16 => {
                return call_closure_boxed_16(closure, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            else => @panic("unhandled branch"),
        }
    } else if (arity < fixed.len + 16) {
        var args: [16]Box = undefined;
        for (0..fixed.len) |i| {
            args[i] = fixed[i];
        }
        args[fixed.len + 0] = arg0;
        args[fixed.len + 1] = arg1;
        args[fixed.len + 2] = arg2;
        args[fixed.len + 3] = arg3;
        args[fixed.len + 4] = arg4;
        args[fixed.len + 5] = arg5;
        args[fixed.len + 6] = arg6;
        args[fixed.len + 7] = arg7;
        args[fixed.len + 8] = arg8;
        args[fixed.len + 9] = arg9;
        args[fixed.len + 10] = arg10;
        args[fixed.len + 11] = arg11;
        args[fixed.len + 12] = arg12;
        args[fixed.len + 13] = arg13;
        args[fixed.len + 14] = arg14;
        args[fixed.len + 15] = arg15;
        const res = call_closure_boxed_n(pap.closure, args[0..arity]);
        return apply_boxed_n(res.as_object(), args[arity..]);
    } else {
        const newPap = Object.allocPap(pap.closure, arity, @intCast(fixed.len + 16));
        for (0..fixed.len) |i| {
            newPap.fixed()[i] = fixed[i];
        }
        newPap.fixed()[fixed.len + 0] = arg0;
        newPap.fixed()[fixed.len + 1] = arg1;
        newPap.fixed()[fixed.len + 2] = arg2;
        newPap.fixed()[fixed.len + 3] = arg3;
        newPap.fixed()[fixed.len + 4] = arg4;
        newPap.fixed()[fixed.len + 5] = arg5;
        newPap.fixed()[fixed.len + 6] = arg6;
        newPap.fixed()[fixed.len + 7] = arg7;
        newPap.fixed()[fixed.len + 8] = arg8;
        newPap.fixed()[fixed.len + 9] = arg9;
        newPap.fixed()[fixed.len + 10] = arg10;
        newPap.fixed()[fixed.len + 11] = arg11;
        newPap.fixed()[fixed.len + 12] = arg12;
        newPap.fixed()[fixed.len + 13] = arg13;
        newPap.fixed()[fixed.len + 14] = arg14;
        newPap.fixed()[fixed.len + 15] = arg15;
        return Object.castFrom(newPap).box();
    }
}
pub inline fn apply_closure_boxed_1(f: *Closure, arg0: Box) Box {
    const arity = f.arity();
    if (1 == arity) {
        return call_closure_boxed_1(f, arg0);
    }
    const pap = Object.allocPap(f, arity, 1);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_2(f: *Closure, arg0: Box, arg1: Box) Box {
    const arity = f.arity();
    if (2 == arity) {
        return call_closure_boxed_2(f, arg0, arg1);
    }
    if (2 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_1(res.as_object(), arg1);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 2);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_3(f: *Closure, arg0: Box, arg1: Box, arg2: Box) Box {
    const arity = f.arity();
    if (3 == arity) {
        return call_closure_boxed_3(f, arg0, arg1, arg2);
    }
    if (3 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_2(res.as_object(), arg1, arg2);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_1(res.as_object(), arg2);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 3);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_4(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box) Box {
    const arity = f.arity();
    if (4 == arity) {
        return call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
    }
    if (4 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_3(res.as_object(), arg1, arg2, arg3);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_2(res.as_object(), arg2, arg3);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_1(res.as_object(), arg3);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 4);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_5(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box) Box {
    const arity = f.arity();
    if (5 == arity) {
        return call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
    }
    if (5 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_4(res.as_object(), arg1, arg2, arg3, arg4);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_3(res.as_object(), arg2, arg3, arg4);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_2(res.as_object(), arg3, arg4);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_1(res.as_object(), arg4);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 5);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_6(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box) Box {
    const arity = f.arity();
    if (6 == arity) {
        return call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
    }
    if (6 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_5(res.as_object(), arg1, arg2, arg3, arg4, arg5);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_4(res.as_object(), arg2, arg3, arg4, arg5);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_3(res.as_object(), arg3, arg4, arg5);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_2(res.as_object(), arg4, arg5);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_1(res.as_object(), arg5);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 6);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_7(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box) Box {
    const arity = f.arity();
    if (7 == arity) {
        return call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
    }
    if (7 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_6(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_5(res.as_object(), arg2, arg3, arg4, arg5, arg6);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_4(res.as_object(), arg3, arg4, arg5, arg6);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_3(res.as_object(), arg4, arg5, arg6);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_2(res.as_object(), arg5, arg6);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_1(res.as_object(), arg6);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 7);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_8(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box) Box {
    const arity = f.arity();
    if (8 == arity) {
        return call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    }
    if (8 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_7(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_6(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_5(res.as_object(), arg3, arg4, arg5, arg6, arg7);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_4(res.as_object(), arg4, arg5, arg6, arg7);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_3(res.as_object(), arg5, arg6, arg7);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_2(res.as_object(), arg6, arg7);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_1(res.as_object(), arg7);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 8);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_9(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box) Box {
    const arity = f.arity();
    if (9 == arity) {
        return call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    }
    if (9 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_8(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_7(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_6(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_5(res.as_object(), arg4, arg5, arg6, arg7, arg8);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_4(res.as_object(), arg5, arg6, arg7, arg8);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_3(res.as_object(), arg6, arg7, arg8);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_2(res.as_object(), arg7, arg8);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_1(res.as_object(), arg8);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 9);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_10(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box) Box {
    const arity = f.arity();
    if (10 == arity) {
        return call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    }
    if (10 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_9(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_8(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_7(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_6(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_5(res.as_object(), arg5, arg6, arg7, arg8, arg9);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_4(res.as_object(), arg6, arg7, arg8, arg9);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_3(res.as_object(), arg7, arg8, arg9);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_2(res.as_object(), arg8, arg9);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_1(res.as_object(), arg9);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 10);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_11(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box) Box {
    const arity = f.arity();
    if (11 == arity) {
        return call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    }
    if (11 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_10(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_9(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_8(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_7(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_6(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_5(res.as_object(), arg6, arg7, arg8, arg9, arg10);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_4(res.as_object(), arg7, arg8, arg9, arg10);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_3(res.as_object(), arg8, arg9, arg10);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_2(res.as_object(), arg9, arg10);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_1(res.as_object(), arg10);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 11);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_12(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box) Box {
    const arity = f.arity();
    if (12 == arity) {
        return call_closure_boxed_12(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    }
    if (12 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_11(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_10(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_9(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_8(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_7(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10, arg11);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_6(res.as_object(), arg6, arg7, arg8, arg9, arg10, arg11);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_5(res.as_object(), arg7, arg8, arg9, arg10, arg11);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_4(res.as_object(), arg8, arg9, arg10, arg11);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_3(res.as_object(), arg9, arg10, arg11);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_2(res.as_object(), arg10, arg11);
            },
            11 => {
                const res = call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
                return apply_boxed_1(res.as_object(), arg11);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 12);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    fixed[11] = arg11;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_13(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box) Box {
    const arity = f.arity();
    if (13 == arity) {
        return call_closure_boxed_13(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
    }
    if (13 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_12(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_11(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_10(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_9(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_8(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_7(res.as_object(), arg6, arg7, arg8, arg9, arg10, arg11, arg12);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_6(res.as_object(), arg7, arg8, arg9, arg10, arg11, arg12);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_5(res.as_object(), arg8, arg9, arg10, arg11, arg12);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_4(res.as_object(), arg9, arg10, arg11, arg12);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_3(res.as_object(), arg10, arg11, arg12);
            },
            11 => {
                const res = call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
                return apply_boxed_2(res.as_object(), arg11, arg12);
            },
            12 => {
                const res = call_closure_boxed_12(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
                return apply_boxed_1(res.as_object(), arg12);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 13);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    fixed[11] = arg11;
    fixed[12] = arg12;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_14(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box) Box {
    const arity = f.arity();
    if (14 == arity) {
        return call_closure_boxed_14(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
    }
    if (14 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_13(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_12(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_11(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_10(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_9(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_8(res.as_object(), arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_7(res.as_object(), arg7, arg8, arg9, arg10, arg11, arg12, arg13);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_6(res.as_object(), arg8, arg9, arg10, arg11, arg12, arg13);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_5(res.as_object(), arg9, arg10, arg11, arg12, arg13);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_4(res.as_object(), arg10, arg11, arg12, arg13);
            },
            11 => {
                const res = call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
                return apply_boxed_3(res.as_object(), arg11, arg12, arg13);
            },
            12 => {
                const res = call_closure_boxed_12(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
                return apply_boxed_2(res.as_object(), arg12, arg13);
            },
            13 => {
                const res = call_closure_boxed_13(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
                return apply_boxed_1(res.as_object(), arg13);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 14);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    fixed[11] = arg11;
    fixed[12] = arg12;
    fixed[13] = arg13;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_15(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box) Box {
    const arity = f.arity();
    if (15 == arity) {
        return call_closure_boxed_15(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
    }
    if (15 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_14(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_13(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_12(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_11(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_10(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_9(res.as_object(), arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_8(res.as_object(), arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_7(res.as_object(), arg8, arg9, arg10, arg11, arg12, arg13, arg14);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_6(res.as_object(), arg9, arg10, arg11, arg12, arg13, arg14);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_5(res.as_object(), arg10, arg11, arg12, arg13, arg14);
            },
            11 => {
                const res = call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
                return apply_boxed_4(res.as_object(), arg11, arg12, arg13, arg14);
            },
            12 => {
                const res = call_closure_boxed_12(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
                return apply_boxed_3(res.as_object(), arg12, arg13, arg14);
            },
            13 => {
                const res = call_closure_boxed_13(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
                return apply_boxed_2(res.as_object(), arg13, arg14);
            },
            14 => {
                const res = call_closure_boxed_14(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
                return apply_boxed_1(res.as_object(), arg14);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 15);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    fixed[11] = arg11;
    fixed[12] = arg12;
    fixed[13] = arg13;
    fixed[14] = arg14;
    return Object.castFrom(pap).box();
}
pub inline fn apply_closure_boxed_16(f: *Closure, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box, arg15: Box) Box {
    const arity = f.arity();
    if (16 == arity) {
        return call_closure_boxed_16(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
    }
    if (16 > arity) {
        switch (arity) {
            1 => {
                const res = call_closure_boxed_1(f, arg0);
                return apply_boxed_15(res.as_object(), arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            2 => {
                const res = call_closure_boxed_2(f, arg0, arg1);
                return apply_boxed_14(res.as_object(), arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            3 => {
                const res = call_closure_boxed_3(f, arg0, arg1, arg2);
                return apply_boxed_13(res.as_object(), arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            4 => {
                const res = call_closure_boxed_4(f, arg0, arg1, arg2, arg3);
                return apply_boxed_12(res.as_object(), arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            5 => {
                const res = call_closure_boxed_5(f, arg0, arg1, arg2, arg3, arg4);
                return apply_boxed_11(res.as_object(), arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            6 => {
                const res = call_closure_boxed_6(f, arg0, arg1, arg2, arg3, arg4, arg5);
                return apply_boxed_10(res.as_object(), arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            7 => {
                const res = call_closure_boxed_7(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
                return apply_boxed_9(res.as_object(), arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            8 => {
                const res = call_closure_boxed_8(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
                return apply_boxed_8(res.as_object(), arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            9 => {
                const res = call_closure_boxed_9(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
                return apply_boxed_7(res.as_object(), arg9, arg10, arg11, arg12, arg13, arg14, arg15);
            },
            10 => {
                const res = call_closure_boxed_10(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
                return apply_boxed_6(res.as_object(), arg10, arg11, arg12, arg13, arg14, arg15);
            },
            11 => {
                const res = call_closure_boxed_11(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
                return apply_boxed_5(res.as_object(), arg11, arg12, arg13, arg14, arg15);
            },
            12 => {
                const res = call_closure_boxed_12(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
                return apply_boxed_4(res.as_object(), arg12, arg13, arg14, arg15);
            },
            13 => {
                const res = call_closure_boxed_13(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
                return apply_boxed_3(res.as_object(), arg13, arg14, arg15);
            },
            14 => {
                const res = call_closure_boxed_14(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
                return apply_boxed_2(res.as_object(), arg14, arg15);
            },
            15 => {
                const res = call_closure_boxed_15(f, arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14);
                return apply_boxed_1(res.as_object(), arg15);
            },
            else => @panic("unhandled branch"),
        }
    }
    const pap = Object.allocPap(f, arity, 16);
    const fixed = pap.fixed();
    fixed[0] = arg0;
    fixed[1] = arg1;
    fixed[2] = arg2;
    fixed[3] = arg3;
    fixed[4] = arg4;
    fixed[5] = arg5;
    fixed[6] = arg6;
    fixed[7] = arg7;
    fixed[8] = arg8;
    fixed[9] = arg9;
    fixed[10] = arg10;
    fixed[11] = arg11;
    fixed[12] = arg12;
    fixed[13] = arg13;
    fixed[14] = arg14;
    fixed[15] = arg15;
    return Object.castFrom(pap).box();
}
pub fn apply_boxed_1(f: *Object, arg0: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_1(f.cast(Closure), arg0),
        .Pap => return apply_pap_boxed_1(f.cast(Pap), arg0),
    }
}
pub fn apply_boxed_2(f: *Object, arg0: Box, arg1: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_2(f.cast(Closure), arg0, arg1),
        .Pap => return apply_pap_boxed_2(f.cast(Pap), arg0, arg1),
    }
}
pub fn apply_boxed_3(f: *Object, arg0: Box, arg1: Box, arg2: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_3(f.cast(Closure), arg0, arg1, arg2),
        .Pap => return apply_pap_boxed_3(f.cast(Pap), arg0, arg1, arg2),
    }
}
pub fn apply_boxed_4(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_4(f.cast(Closure), arg0, arg1, arg2, arg3),
        .Pap => return apply_pap_boxed_4(f.cast(Pap), arg0, arg1, arg2, arg3),
    }
}
pub fn apply_boxed_5(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_5(f.cast(Closure), arg0, arg1, arg2, arg3, arg4),
        .Pap => return apply_pap_boxed_5(f.cast(Pap), arg0, arg1, arg2, arg3, arg4),
    }
}
pub fn apply_boxed_6(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_6(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5),
        .Pap => return apply_pap_boxed_6(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5),
    }
}
pub fn apply_boxed_7(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_7(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6),
        .Pap => return apply_pap_boxed_7(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6),
    }
}
pub fn apply_boxed_8(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_8(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7),
        .Pap => return apply_pap_boxed_8(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7),
    }
}
pub fn apply_boxed_9(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_9(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8),
        .Pap => return apply_pap_boxed_9(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8),
    }
}
pub fn apply_boxed_10(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_10(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9),
        .Pap => return apply_pap_boxed_10(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9),
    }
}
pub fn apply_boxed_11(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_11(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10),
        .Pap => return apply_pap_boxed_11(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10),
    }
}
pub fn apply_boxed_12(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_12(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11),
        .Pap => return apply_pap_boxed_12(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11),
    }
}
pub fn apply_boxed_13(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_13(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12),
        .Pap => return apply_pap_boxed_13(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12),
    }
}
pub fn apply_boxed_14(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_14(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13),
        .Pap => return apply_pap_boxed_14(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13),
    }
}
pub fn apply_boxed_15(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_15(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14),
        .Pap => return apply_pap_boxed_15(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14),
    }
}
pub fn apply_boxed_16(f: *Object, arg0: Box, arg1: Box, arg2: Box, arg3: Box, arg4: Box, arg5: Box, arg6: Box, arg7: Box, arg8: Box, arg9: Box, arg10: Box, arg11: Box, arg12: Box, arg13: Box, arg14: Box, arg15: Box) Box {
    switch (f.tag) {
        .Fun => return apply_closure_boxed_16(f.cast(Closure), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15),
        .Pap => return apply_pap_boxed_16(f.cast(Pap), arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15),
    }
}
pub fn call_closure_boxed_n(f: *Closure, args: []Box) Box {
    switch (args.len) {
        1 => {
            return call_closure_boxed_1(f, args[0]);
        },
        2 => {
            return call_closure_boxed_2(f, args[0], args[1]);
        },
        3 => {
            return call_closure_boxed_3(f, args[0], args[1], args[2]);
        },
        4 => {
            return call_closure_boxed_4(f, args[0], args[1], args[2], args[3]);
        },
        5 => {
            return call_closure_boxed_5(f, args[0], args[1], args[2], args[3], args[4]);
        },
        6 => {
            return call_closure_boxed_6(f, args[0], args[1], args[2], args[3], args[4], args[5]);
        },
        7 => {
            return call_closure_boxed_7(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        },
        8 => {
            return call_closure_boxed_8(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        },
        9 => {
            return call_closure_boxed_9(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
        },
        10 => {
            return call_closure_boxed_10(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
        },
        11 => {
            return call_closure_boxed_11(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
        },
        12 => {
            return call_closure_boxed_12(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
        },
        13 => {
            return call_closure_boxed_13(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
        },
        14 => {
            return call_closure_boxed_14(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
        },
        15 => {
            return call_closure_boxed_15(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
        },
        16 => {
            return call_closure_boxed_16(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
        },
        else => {
            debug.assert(f.arity() > 16);
            const code: *FnBoxedN = @ptrCast(f.code);
            return code(f, args.ptr);
        },
    }
}
pub fn apply_boxed_n(f: *Object, args: []Box) Box {
    switch (args.len) {
        1 => {
            return apply_boxed_1(f, args[0]);
        },
        2 => {
            return apply_boxed_2(f, args[0], args[1]);
        },
        3 => {
            return apply_boxed_3(f, args[0], args[1], args[2]);
        },
        4 => {
            return apply_boxed_4(f, args[0], args[1], args[2], args[3]);
        },
        5 => {
            return apply_boxed_5(f, args[0], args[1], args[2], args[3], args[4]);
        },
        6 => {
            return apply_boxed_6(f, args[0], args[1], args[2], args[3], args[4], args[5]);
        },
        7 => {
            return apply_boxed_7(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
        },
        8 => {
            return apply_boxed_8(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
        },
        9 => {
            return apply_boxed_9(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8]);
        },
        10 => {
            return apply_boxed_10(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9]);
        },
        11 => {
            return apply_boxed_11(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10]);
        },
        12 => {
            return apply_boxed_12(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11]);
        },
        13 => {
            return apply_boxed_13(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12]);
        },
        14 => {
            return apply_boxed_14(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13]);
        },
        15 => {
            return apply_boxed_15(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14]);
        },
        16 => {
            return apply_boxed_16(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
        },
        else => {
            const res = apply_boxed_16(f, args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7], args[8], args[9], args[10], args[11], args[12], args[13], args[14], args[15]);
            return apply_boxed_n(res.as_object(), args[16..]);
        },
    }
}
