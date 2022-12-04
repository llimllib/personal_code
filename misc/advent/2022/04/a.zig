// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const Vec = struct {
    a: i16,
    b: i16,
};

const VectorPair = struct {
    a: Vec,
    b: Vec,
};

const VectorList = std.ArrayList(VectorPair);

// returns true if one vector completely overlaps the other
pub fn overlap(v: VectorPair) bool {
    return (v.a.a <= v.b.a and v.a.b >= v.b.b) or (v.b.a <= v.a.a and v.b.b >= v.a.b);
}

pub fn score(vectors: VectorList) !usize {
    var overlaps: usize = 0;
    for (vectors.items) |v| {
        if (overlap(v)) {
            overlaps += 1;
        }
    }
    return overlaps;
}

// returns true if either vector partially intersects the other
pub fn between(v: VectorPair) bool {
    return (v.b.a <= v.a.a and v.a.a <= v.b.b) or (v.b.a <= v.a.b and v.a.b <= v.b.b);
}

pub fn score2(vectors: VectorList) !usize {
    var overlaps: usize = 0;
    for (vectors.items) |v| {
        if (between(v) or overlap(v)) {
            overlaps += 1;
        }
    }
    return overlaps;
}

pub fn parse(data: []const u8) !VectorList {
    var list = VectorList.init(gpa);
    var tdata = std.mem.trim(u8, data, " \n");
    var lines = std.mem.split(u8, tdata, "\n");
    while (lines.next()) |line| {
        var parts = std.mem.tokenize(u8, line, "-,");
        var a = try std.fmt.parseInt(i16, parts.next().?, 10);
        var b = try std.fmt.parseInt(i16, parts.next().?, 10);
        var c = try std.fmt.parseInt(i16, parts.next().?, 10);
        var d = try std.fmt.parseInt(i16, parts.next().?, 10);
        try list.append(.{
            .a = .{
                .a = a,
                .b = b,
            },
            .b = .{
                .a = c,
                .b = d,
            },
        });
    }
    return list;
}

pub fn main() !void {
    var entries = try parse(input);
    std.debug.print("part 1: {}\n", .{try score(entries)});
    std.debug.print("part 2: {}\n", .{try score2(entries)});
}

// zig test a.zig
test "part 1" {
    var result = try score(try parse(
        \\2-4,6-8
        \\2-3,4-5
        \\5-7,7-9
        \\2-8,3-7
        \\6-6,4-6
        \\2-6,4-8
    ));
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 2);
}

test "part 2" {
    var result = try score2(try parse(
        \\2-4,6-8
        \\2-3,4-5
        \\5-7,7-9
        \\2-8,3-7
        \\6-6,4-6
        \\2-6,4-8
    ));
    std.debug.print("{d}\n", .{result});
    try testing.expect(result == 4);
}
