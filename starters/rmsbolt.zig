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

// Functions marked with `export` use the C calling convention, so its parameters and
// return value can only have C types.
// To export a native Zig fn, use the following pattern:
fn zigFn(xs: []u8) []u8 {
    for (xs) |*x| {
        x.* *= 2;
    }
    return xs;
}

export fn exportZigFn() usize {
    return @ptrToInt(zigFn);
}

// In some cases, Zig embeds a panic handler that prints stack traces, causing a
// disassembly much larger than normal.
// You can optionally place this function in files you disassemble to make them easier to digest.
pub fn panic(msg: []const u8, error_return_trace: ?*std.builtin.StackTrace) noreturn {
    _ = msg;
    _ = error_return_trace;
    while (true) {}
}
