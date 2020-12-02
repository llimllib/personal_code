const std = @import("std");
const fs = std.fs;

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    const input = try std.fs.cwd().readFileAlloc(&alloc.allocator, "input.txt", std.math.maxInt(usize));

    var lines = std.mem.tokenize(input, "\n");
    var valid: usize = 0;
    var valid2: usize = 0;
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(line, ": ");
        const reps = parts.next().?;
        const ltr = parts.next().?[0];
        const pw = parts.next().?;
        var minmax = std.mem.tokenize(reps, "-");
        const min = try std.fmt.parseInt(i32, minmax.next().?, 10);
        const max = try std.fmt.parseInt(i32, minmax.next().?, 10);

        // part 1
        var cnt: usize = 0;
        for (pw) |chr| {
            if (chr == ltr) {
                cnt += 1;
            }
        }
        if (min <= cnt and cnt <= max) {
            valid += 1;
        }

        // part 2
        const minidx = @intCast(usize, min - 1);
        const a = pw[minidx] == ltr;
        const maxidx = @intCast(usize, max - 1);
        const b = pw[maxidx] == ltr;
        if ((a and !b) or (!a and b)) {
            valid2 += 1;
        }
    }
    std.debug.print("part 1: {}\npart 2: {}\n", .{ valid, valid2 });
}
