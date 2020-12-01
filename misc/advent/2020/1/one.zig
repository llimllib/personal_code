const std = @import("std");
const fs = std.fs;

pub fn main() !void {
    // open the file
    var pathBuffer: [std.fs.MAX_PATH_BYTES]u8 = undefined;
    const input = try fs.openFileAbsolute(try fs.realpath("input.txt", &pathBuffer), .{ .read = true });

    // read it into a buffer
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    const contents = try input.readToEndAlloc(&alloc.allocator, std.math.maxInt(u32));

    // create a line iterator, then read into an arraylist
    var lines = std.mem.split(contents, "\n");
    var expenses = std.ArrayList(i32).init(&alloc.allocator);
    while (lines.next()) |line| {
        var trimmed = std.mem.trim(u8, line, " \n");
        if (trimmed.len == 0) {
            continue;
        }

        const i = try std.fmt.parseInt(i32, trimmed, 10);
        try expenses.append(i);
    }

    // finally, calculate part 1
    part1: for (expenses.items) |i| {
        for (expenses.items) |j| {
            if (i + j == 2020) {
                std.debug.print("{} {} {}\n", .{ i, j, i * j });
                break :part1;
            }
        }
    }

    // and part 2
    part2: for (expenses.items) |i| {
        for (expenses.items) |j| {
            for (expenses.items) |k| {
                if (i + j + k == 2020) {
                    std.debug.print("{} {} {} {}\n", .{ i, j, k, i * j * k });
                    break :part2;
                }
            }
        }
    }
}
