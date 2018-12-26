const std = @import("std");

// Zig rmsbolt starter file

// Local Variables:
// rmsbolt-command: "zig"
// rmsbolt-disassemble: nil
// End:

fn isRMS(a: u8) u8 {
    switch (a) {
        'R' => {return 1;},
        'M' => {return 2;},
        'S' => {return 3;},
        else => {return 0;},
    }
}

pub fn main() void {
    const a: u8 = 1 + 1;
    if (isRMS(a) != 0) {
        std.debug.warn("{c}\n", a);
    }
}
