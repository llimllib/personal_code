// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

const input = @embedFile("input.txt");

pub fn score(data: []const u8) !usize {
    var packs = std.mem.split(u8, data, "\n");
    var sum: usize = 0;
    while (packs.next()) |pack| {
        if (pack.len == 0) continue;

        var setA = std.bit_set.IntegerBitSet(64).initEmpty();
        for (pack[0 .. pack.len / 2]) |char| {
            setA.set(char - 'A');
        }
        var setB = std.bit_set.IntegerBitSet(64).initEmpty();
        for (pack[pack.len / 2 ..]) |char| {
            setB.set(char - 'A');
        }
        setA.setIntersection(setB);

        var char = setA.findFirstSet().? + 'A';
        if (char - 'A' < 26) {
            sum += char - '@' + 26;
        } else if (char - 'a' < 26) {
            sum += char - '`';
        }
    }
    return sum;
}

pub fn main() !void {
    std.debug.print("part 1: {}\n", .{try score(input)});
}

// zig test a.zig
test "part 1" {
    var result = try score(
        \\vJrwpWtwJgWrhcsFMMfFFhFp
        \\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        \\PmmdzqPrVvPwwTWBwg
        \\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        \\ttgJtRGJQctTZtZT
        \\CrZsJsPPZsGzwwsLwLmpwMDw
    );
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 157);
}
