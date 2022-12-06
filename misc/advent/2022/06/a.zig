// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const Charset = std.bit_set.IntegerBitSet(64);

pub fn find4th(data: []const u8) usize {
    for (data) |_, i| {
        var set = Charset.initEmpty();
        for (data[i .. i + 4]) |cc| {
            set.set(cc - 'a');
        }
        if (set.count() == 4) {
            return i + 4;
        }
    }
    unreachable;
}

pub fn find14th(data: []const u8) usize {
    for (data) |_, i| {
        var set = Charset.initEmpty();
        for (data[i .. i + 14]) |cc| {
            set.set(cc - 'a');
        }
        if (set.count() == 14) {
            return i + 14;
        }
    }
    unreachable;
}

pub fn main() !void {
    std.debug.print("part 1: {}\n", .{find4th(input)});
    std.debug.print("part 2: {}\n", .{find14th(input)});
}

// zig test a.zig
test "part 1" {
    try testing.expect(find4th("bvwbjplbgvbhsrlpgdmjqwftvncz") == 5);
    try testing.expect(find4th("nppdvjthqldpwncqszvftbrmjlhg") == 6);
    try testing.expect(find4th("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 10);
    try testing.expect(find4th("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") == 11);
}

test "part 2" {
    try testing.expect(find14th("mjqjpqmgbljsphdztnvjfqwrcgsmlb") == 19);
    try testing.expect(find14th("bvwbjplbgvbhsrlpgdmjqwftvncz") == 23);
    try testing.expect(find14th("nppdvjthqldpwncqszvftbrmjlhg") == 23);
    try testing.expect(find14th("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") == 29);
}
