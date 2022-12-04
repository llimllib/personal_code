// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

const input = @embedFile("input.txt");

pub fn overlap(x: usize, y: usize, low: usize, hi: usize) bool {
    return x <= low and y >= hi;
}

pub fn score(data: []const u8) !usize {
    var overlaps: usize = 0;
    var tdata = std.mem.trim(u8, data, " \n");
    var lines = std.mem.split(u8, tdata, "\n");
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(u8, line, "-,");
        var a = try std.fmt.parseInt(usize, parts.next().?, 10);
        var b = try std.fmt.parseInt(usize, parts.next().?, 10);
        var c = try std.fmt.parseInt(usize, parts.next().?, 10);
        var d = try std.fmt.parseInt(usize, parts.next().?, 10);
        // assumes they're sorted least to greatest
        if (overlap(a, b, c, d) or overlap(c, d, a, b)) {
            overlaps += 1;
        }
    }
    return overlaps;
}

pub fn between(x: usize, low: usize, hi: usize) bool {
    return low <= x and x <= hi;
}

pub fn score2(data: []const u8) !usize {
    var overlaps: usize = 0;
    var tdata = std.mem.trim(u8, data, " \n");
    var lines = std.mem.split(u8, tdata, "\n");
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(u8, line, "-,");
        var a = try std.fmt.parseInt(usize, parts.next().?, 10);
        var b = try std.fmt.parseInt(usize, parts.next().?, 10);
        var c = try std.fmt.parseInt(usize, parts.next().?, 10);
        var d = try std.fmt.parseInt(usize, parts.next().?, 10);
        // assumes they're sorted least to greatest
        if (between(a, c, d) or between(b, c, d) or overlap(a, b, c, d)) {
            overlaps += 1;
        }
    }
    return overlaps;
}

pub fn main() !void {
    std.debug.print("part 1: {}\n", .{try score(input)});
    std.debug.print("part 2: {}\n", .{try score2(input)});
}

// zig test a.zig
test "part 1" {
    var result = try score(
        \\2-4,6-8
        \\2-3,4-5
        \\5-7,7-9
        \\2-8,3-7
        \\6-6,4-6
        \\2-6,4-8
    );
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 2);
}

test "part 2" {
    var result = try score2(
        \\2-4,6-8
        \\2-3,4-5
        \\5-7,7-9
        \\2-8,3-7
        \\6-6,4-6
        \\2-6,4-8
    );
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 4);
}
