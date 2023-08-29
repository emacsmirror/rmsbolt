const std = @import("std");

// Zig rmsbolt starter file

// Local Variables:
// rmsbolt-command: "zig build-obj -O ReleaseFast"
// rmsbolt-disassemble: nil
// End:

export fn isRMS(a: u8) u8 {
    return switch (a) {
        'R' => 1,
        'M' => 2,
        'S' => 3,
        else => 0,
    };
}

// Exported by `exportPubFns` below
pub fn zigFn(xs: []u8) []u8 {
    for (xs) |*x| {
        x.* *= 2;
    }
    return xs;
}

// Export all public, non-generic functions in this file.
// This is needed because functions that accept or return Zig-specific types can't be marked
// with `export`.
// `export` is limited to functions that only accept or return C types, which makes them
// compatible with the C calling convention.
export fn exportPubFns() usize {
    var fns: usize = 0;
    inline for (@typeInfo((@This())).Struct.decls) |decl| {
        const field = @field(@This(), decl.name);
        const info = @typeInfo(@TypeOf(field));
        if (info == .Fn and !info.Fn.is_generic) {
            fns += @intFromPtr(&field);
        }
    }
    return fns;
}

// In some cases, Zig embeds a panic handler that prints stack traces, causing a
// disassembly much larger than normal.
// You can optionally place this function in files you disassemble to make them easier to digest.
pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace, ret_addr: ?usize) noreturn {
    _ = msg;
    _ = error_return_trace;
    _ = ret_addr;
    while (true) {}
}
