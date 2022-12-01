// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

const input = @embedFile("input.txt");

fn gt(_: void, lhs: usize, rhs: usize) bool {
    return lhs > rhs;
}

pub fn calories(data: []const u8) ![3]usize {
    var elves = std.mem.split(u8, data, "\n\n");

    var maxes = [_]usize{ 0, 0, 0 };
    while (elves.next()) |elf| {
        var foods = std.mem.tokenize(u8, elf, "\n");
        var sum: usize = 0;
        while (foods.next()) |food| {
            sum += try std.fmt.parseInt(usize, food, 10);
        }
        if (sum > maxes[2]) {
            maxes[2] = sum;
            std.sort.sort(usize, &maxes, {}, gt);
        }
    }

    return maxes;
}

pub fn main() !void {
    var maxes = try calories(input);

    std.debug.print("part 1: {}\n", .{maxes[0]});
    std.debug.print("part 2: {}\n", .{maxes[0] + maxes[1] + maxes[2]});
}

// zig test a.zig
test "sample" {
    var result = try calories(
        \\1000
        \\2000
        \\3000
        \\
        \\4000
        \\
        \\5000
        \\6000
        \\
        \\7000
        \\8000
        \\9000
        \\
        \\10000
    );
    try testing.expect(std.mem.eql(usize, &result, &[3]usize{ 24000, 11000, 10000 }));
}
