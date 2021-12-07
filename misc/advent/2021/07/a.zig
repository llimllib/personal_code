// To run:
// zig run a.zig -- input.txt
//
// To compile and run:
// zig build-exe ./a.zig -O ReleaseSmall
// ./a input.txt
const std = @import("std");

const stdout = std.io.getStdOut().writer();

fn parse(input: []const u8, alloc: std.mem.Allocator) !std.ArrayList(i32) {
    var ns = std.ArrayList(i32).init(alloc);

    var trimmed = std.mem.trim(u8, input, "\n");
    var nums = std.mem.split(u8, trimmed, ",");

    while (nums.next()) |n| {
        try ns.append(try std.fmt.parseInt(i32, n, 10));
    }

    return ns;
}

fn abs(n: i32) i32 {
    if (n < 0) return -n;
    return n;
}

fn minfuel(ns: std.ArrayList(i32)) !void {
    var min: i32 = std.math.maxInt(i32);
    var max: i32 = std.math.minInt(i32);
    for (ns.items) |n| {
        if (n < min)
            min = n;
        if (n > max)
            max = n;
    }

    var start = min;
    var minn: i32 = 0;
    var minsum: i32 = std.math.maxInt(i32);
    while (start < max) : (start += 1) {
        var sum: i32 = 0;
        for (ns.items) |n| {
            sum += abs(start - n);
        }
        if (sum < minsum) {
            minsum = sum;
            minn = start;
        }
    }
    try stdout.print("min {} {}\n", .{ minn, minsum });
}

fn cost(n: i32) i32 {
    return @divFloor(n * (n + 1), 2);
}

fn minfuel2(ns: std.ArrayList(i32)) !void {
    var min: i32 = std.math.maxInt(i32);
    var max: i32 = std.math.minInt(i32);
    for (ns.items) |n| {
        if (n < min)
            min = n;
        if (n > max)
            max = n;
    }

    var start = min;
    var minn: i32 = 0;
    var minsum: i32 = std.math.maxInt(i32);
    while (start < max) : (start += 1) {
        var sum: i32 = 0;
        for (ns.items) |n| {
            sum += cost(abs(start - n));
        }
        if (sum < minsum) {
            minsum = sum;
            minn = start;
        }
    }
    try stdout.print("min {} {}\n", .{ minn, minsum });
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    var ns = try parse(input, allocator);
    try minfuel(ns);
    try minfuel2(ns);
}
