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

pub fn score2(data: []const u8) !usize {
    var packs = std.mem.split(u8, data, "\n");
    var sum: usize = 0;
    while (packs.next()) |pack1| {
        if (pack1.len == 0) continue;

        var pack2 = packs.next().?;
        var pack3 = packs.next().?;
        var setA = std.bit_set.IntegerBitSet(64).initEmpty();
        for (pack1) |char| {
            setA.set(char - 'A');
        }
        var setB = std.bit_set.IntegerBitSet(64).initEmpty();
        for (pack2) |char| {
            setB.set(char - 'A');
        }
        var setC = std.bit_set.IntegerBitSet(64).initEmpty();
        for (pack3) |char| {
            setC.set(char - 'A');
        }
        setA.setIntersection(setB);
        setA.setIntersection(setC);

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
    std.debug.print("part 2: {}\n", .{try score2(input)});
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

test "part 2" {
    var result = try score2(
        \\vJrwpWtwJgWrhcsFMMfFFhFp
        \\jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        \\PmmdzqPrVvPwwTWBwg
        \\wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        \\ttgJtRGJQctTZtZT
        \\CrZsJsPPZsGzwwsLwLmpwMDw
    );
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 70);
}
