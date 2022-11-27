// run with:
// zig build-exe a.zig && ./a

const std = @import("std");

// embed the input data into the binary
// const data = @embedFile("sample.txt");
const data = @embedFile("input.txt");

pub fn main() !void {
    var lines = std.mem.tokenize(u8, data, "\r\n");

    var pos: i32 = 0;
    var depth: i32 = 0;
    while (lines.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.split(u8, line, " ");
        var cmd = parts.next().?;
        var n = try std.fmt.parseInt(i32, parts.next().?, 10);

        if (std.mem.eql(u8, cmd, "forward")) {
            pos += n;
        } else if (std.mem.eql(u8, cmd, "up")) {
            depth -= n;
        } else if (std.mem.eql(u8, cmd, "down")) {
            depth += n;
        } else {
            unreachable;
        }
    }

    std.debug.print("part 1: {}\n", .{pos * depth});

    var lines2 = std.mem.tokenize(u8, data, "\r\n");
    var aim: i32 = 0;
    pos = 0;
    depth = 0;
    while (lines2.next()) |line| {
        if (line.len == 0) continue;

        var parts = std.mem.split(u8, line, " ");
        var cmd = parts.next().?;
        var n = try std.fmt.parseInt(i32, parts.next().?, 10);

        if (std.mem.eql(u8, cmd, "forward")) {
            pos += n;
            depth += aim * n;
        } else if (std.mem.eql(u8, cmd, "up")) {
            aim -= n;
        } else if (std.mem.eql(u8, cmd, "down")) {
            aim += n;
        } else {
            unreachable;
        }
    }

    std.debug.print("part 2: {}\n", .{pos * depth});
}
