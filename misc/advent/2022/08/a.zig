// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const seenSet = std.bit_set.ArrayBitSet(u64, 100 * 100);
const Map = struct {
    map: std.ArrayList(std.ArrayList(u8)),
    width: usize,
    height: usize,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator, data: []const u8) Map {
        var map: std.ArrayList(std.ArrayList(u8)) = std.ArrayList(std.ArrayList(u8)).initCapacity(alloc, 100) catch unreachable;
        var lines = std.mem.split(u8, data, "\n");
        while (lines.next()) |row| {
            if (row.len == 0) continue;

            var rowlist = std.ArrayList(u8).initCapacity(alloc, row.len) catch unreachable;
            for (row) |c| {
                rowlist.append(c) catch unreachable;
            }
            map.append(rowlist) catch unreachable;
        }
        return Map{
            .map = map,
            .width = map.items[0].items.len,
            .height = map.items.len,
            .alloc = alloc,
        };
    }

    pub fn score(self: *Map) usize {
        var count: usize = 0;
        var seen: seenSet = seenSet.initEmpty();

        // count the rows direction
        for (self.map.items) |row, i| {
            var max: usize = 0;
            for (row.items) |c, j| {
                if (c > max) {
                    count += 1;
                    max = c;
                    seen.set(i * self.width + j);
                }
            }

            // now count rows backwards
            var k = self.width;
            max = 0;
            while (k > 0) : (k -= 1) {
                var idx = k - 1;
                // std.debug.print("{d} {c} {d}\n", .{ idx, row.items[idx], max });
                if (row.items[idx] > max and !seen.isSet(i * self.width + idx)) {
                    count += 1;
                    max = row.items[idx];
                    seen.set(i * self.width + idx);
                }
            }
        }

        // count the columns
        var col: usize = 0;
        while (col < self.width) : (col += 1) {
            var row: usize = 0;
            var max: usize = 0;
            // row forwards
            while (row < self.height) : (row += 1) {
                if (self.map.items[row].items[col] > max and !seen.isSet(row * self.width + col)) {
                    count += 1;
                    max = self.map.items[row].items[col];
                    seen.set(row * self.width + col);
                }
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
