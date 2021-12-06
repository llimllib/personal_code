// day 5. Some stuff stolen from https://github.com/SpexGuy/Advent2021/blob/main/src/day05.zig
// not complete, still just re-learning
const std = @import("std");

const stdout = std.io.getStdOut().writer();

const Point = struct { x: i32, y: i32 };
const Map = std.AutoHashMap(Point, bool);

const Line = struct {
    x1: i32,
    y1: i32,
    x2: i32,
    y2: i32,

    pub fn draw(self: Line, map: *Map) !void {
        if (self.x1 == self.x2) {
            var start = std.math.min(self.y1, self.y2);
            const end = std.math.max(self.y1, self.y2);
            while (start <= end) : (start += 1) {
                var pt = Point{ .x = self.x1, .y = start };
                try map.put(pt, map.contains(pt));
            }
        } else if (self.y1 == self.y2) {
            var start = std.math.min(self.y1, self.y2);
            const end = std.math.max(self.y1, self.y2);
            while (start <= end) : (start += 1) {
                var pt = Point{ .x = self.x1, .y = start };
                try map.put(pt, map.contains(pt));
            }
        } else {
            // XXX: not done
            var start = std.math.min(self.y1, self.y2);
            const end = std.math.max(self.y1, self.y2);
            while (start <= end) : (start += 1) {
                var pt = Point{ .x = self.x1, .y = start };
                try map.put(pt, map.contains(pt));
            }
        }
    }
};
const Lines = std.ArrayList(Line);

fn parse(input: []const u8, alloc: std.mem.Allocator) !Lines {
    var lines = Lines.init(alloc);

    var trimmed = std.mem.trim(u8, input, "\n");
    var inputlines = std.mem.split(u8, trimmed, "\n");
    while (inputlines.next()) |iline| {
        var bits = std.mem.tokenize(u8, iline, ", ->");
        var line = try lines.addOne();
        line.x1 = try std.fmt.parseInt(i32, bits.next().?, 10);
        line.y1 = try std.fmt.parseInt(i32, bits.next().?, 10);
        line.x2 = try std.fmt.parseInt(i32, bits.next().?, 10);
        line.y2 = try std.fmt.parseInt(i32, bits.next().?, 10);
    }

    return lines;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    var lines = try parse(input, allocator);
    try stdout.print("{}\n", .{lines});
    var map = Map.init(allocator);
    for (lines.items) |line| {
        try line.draw(&map);
    }
}
