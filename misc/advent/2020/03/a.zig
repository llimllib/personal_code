const std = @import("std");
const fs = std.fs;

fn search(input: []u8, xinc: usize, yinc: usize) usize {
    var lines = std.mem.tokenize(input, "\n");
    var w: usize = 0;
    var x: usize = 0;
    var y: usize = yinc - 1;
    var trees: usize = 0;
    while (lines.next()) |line| {
        if (line[x] == '#') {
            trees += 1;
        }
        w = line.len;
        x = (x + xinc) % w;
        while (y > 0) : (y -= 1) {
            _ = lines.next();
        }
        y = yinc - 1;
    }
    return trees;
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var args = try std.process.argsAlloc(&alloc.allocator);
    const input = try std.fs.cwd().readFileAlloc(&alloc.allocator, args[1], std.math.maxInt(usize));

    std.debug.print("{}\n", .{search(input, 3, 1)});
    var part2: usize = search(input, 3, 1) *
        search(input, 1, 1) *
        search(input, 5, 1) *
        search(input, 7, 1) *
        search(input, 1, 2);
    std.debug.print("{}\n", .{part2});
}
