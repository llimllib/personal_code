// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const seenSet = std.bit_set.ArrayBitSet(u64, 100 * 100);
const Map = struct {
    map: [100 * 100]u8,
    width: usize,
    height: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, data: []const u8) Map {
        var lines = std.mem.split(u8, data, "\n");
        var map: [100 * 100]u8 = undefined;
        var width: usize = 0;
        var rowN: usize = 0;
        while (lines.next()) |row| {
            if (row.len == 0) continue;
            width = row.len;

            for (row) |c, colN| map[width * rowN + colN] = c;
            rowN += 1;
        }
        return Map{
            .map = map,
            .width = width,
            .height = rowN,
            .alloc = alloc,
        };
    }

    pub fn score(self: *Map) usize {
        var count: usize = 0;

        var row: usize = 0;
        var col: usize = 0;

        std.debug.print("{d} {d} {c}\n", .{ self.height, self.width, self.map[0] });
        while (row < self.height) : (row += 1) {
            while (col < self.width) : (col += 1) {
                var tree = self.map[row * self.width + col];

                // check the column up
                var c = @intCast(isize, col) - 1;
                var found: bool = true;
                while (c >= 0) : (c -= 1) {
                    var idx = @intCast(usize, c) + row;
                    if (self.map[idx] > tree) {
                        found = false;
                        break;
                    }
                }
                if (found) {
                    count += 1;
                    continue;
                }

                // check the column down
            }
        }

        return count;
    }
};

pub fn main() !void {
    var map = Map.init(gpa, input);
    std.debug.print("part 1: {}\n", .{map.score(input)});
}

// zig test a.zig
test "part 1" {
    var result = Map.init(gpa,
        \\30373
        \\25512
        \\65332
        \\33549
        \\35390
    );
    std.debug.print("{d}\n", .{result.score()});
    try testing.expect(result.score() == 21);
}
