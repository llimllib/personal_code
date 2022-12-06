// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const Charset = std.bit_set.IntegerBitSet(64);

pub fn findnth(data: []const u8, n: usize) usize {
    for (data) |_, i| {
        var set = Charset.initEmpty();
        for (data[i .. i + n]) |cc| {
            set.set(cc - 'a');
        }
        if (set.count() == n) {
            return i + n;
        }
    }
    unreachable;
}

pub fn main() !void {
    std.debug.print("part 1: {}\n", .{findnth(input, 4)});
    std.debug.print("part 2: {}\n", .{findnth(input, 14)});
}

// zig test a.zig
test "part 1" {
    try testing.expect(findnth("bvwbjplbgvbhsrlpgdmjqwftvncz", 4) == 5);
    try testing.expect(findnth("nppdvjthqldpwncqszvftbrmjlhg", 4) == 6);
    try testing.expect(findnth("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 4) == 10);
    try testing.expect(findnth("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 4) == 11);
}

test "part 2" {
    try testing.expect(findnth("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 14) == 19);
    try testing.expect(findnth("bvwbjplbgvbhsrlpgdmjqwftvncz", 14) == 23);
    try testing.expect(findnth("nppdvjthqldpwncqszvftbrmjlhg", 14) == 23);
    try testing.expect(findnth("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 14) == 29);
}
