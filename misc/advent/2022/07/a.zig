// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const FS = struct {
    name: []const u8,
    parent: ?*FS,
    children: std.ArrayList(*FS),
    size: usize,
    alloc: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, parent: ?*FS, size: usize) *FS {
        var node = allocator.create(FS) catch unreachable;
        node.* = FS{
            .name = name,
            .parent = parent,
            .children = std.ArrayList(*FS).init(allocator),
            .size = size,
            .alloc = allocator,
        };
        return node;
    }

    pub fn appendChild(self: *FS, fs: *FS) void {
        self.children.append(fs) catch unreachable;
    }
};

pub fn parse(data: []const u8) *FS {
    var root = FS.init(gpa, "/", null, 0);

    var lines = std.mem.split(u8, data, "\n");
    var curdir = root;

    // the first line in all inputs is "cd /" - we'll just assume we're at the
    // root
    _ = lines.next();

    while (lines.next()) |line| {
        if (line.len == 0) continue;

        if (line.len >= 7 and std.mem.eql(u8, line[0..7], "$ cd ..")) {
            curdir = curdir.parent.?;
        } else if (line.len >= 4 and std.mem.eql(u8, line[0..4], "$ cd")) {
            var name = line[5..];
            var node = FS.init(gpa, name, curdir, 0);
            curdir.appendChild(node);
            curdir = node;
        } else if (line[0] >= '0' and line[0] <= '9') {
            var parts = std.mem.split(u8, line, " ");
            var fsize = std.fmt.parseInt(usize, parts.next().?, 10) catch unreachable;
            var name = parts.next().?;
            var node = FS.init(gpa, name, curdir, fsize);
            curdir.appendChild(node);
        }
    }

    return root;
}

pub fn dirsize(root: *FS) usize {
    var size: usize = 0;
    for (root.children.items) |node| {
        if (node.size == 0) {
            size += dirsize(node);
        }
        size += node.size;
    }
    return size;
}

// NOTE: this assumes root is too big, which is true for our inputs but could
// conceivably be false
pub fn score(root: *FS) usize {
    var size: usize = 0;
    var stack = root.children.clone() catch unreachable;
    while (stack.popOrNull()) |node| {
        if (node.size != 0) continue;
        for (node.children.items) |kid| {
            if (kid.size == 0) stack.append(kid) catch unreachable;
        }

        var dsz = dirsize(node);
        if (dsz < 100000) size += dsz;
    }
    return size;
}

pub fn findRm(root: *FS) usize {
    var size: usize = std.math.maxInt(usize);
    var available: usize = 70000000;
    var unused = available - dirsize(root);
    var needed: usize = 30000000;
    var stack = root.children.clone() catch unreachable;
    while (stack.popOrNull()) |node| {
        if (node.size != 0) continue;
        for (node.children.items) |kid| {
            if (kid.size == 0) stack.append(kid) catch unreachable;
        }

        var dsz = dirsize(node);
        if (dsz < size and unused + dsz > needed) size = dsz;
    }
    return size;
}

pub fn main() !void {
    var fs = parse(input);
    std.debug.print("part 1: {d}\n", .{score(fs)});
    std.debug.print("part 2: {d}\n", .{findRm(fs)});
}

// zig test a.zig
test "part 1" {
    var fs = parse(
        \\$ cd /
        \\$ ls
        \\dir a
        \\14848514 b.txt
        \\8504156 c.dat
        \\dir d
        \\$ cd a
        \\$ ls
        \\dir e
        \\29116 f
        \\2557 g
        \\62596 h.lst
        \\$ cd e
        \\$ ls
        \\584 i
        \\$ cd ..
        \\$ cd ..
        \\$ cd d
        \\$ ls
        \\4060174 j
        \\8033020 d.log
        \\5626152 d.ext
        \\7214296 k
    );
    var result = score(fs);
    std.debug.print("result: {d}\n", .{result});
    try testing.expect(result == 95437);
}

test "part 2" {
    var fs = parse(
        \\$ cd /
        \\$ ls
        \\dir a
        \\14848514 b.txt
        \\8504156 c.dat
        \\dir d
        \\$ cd a
        \\$ ls
        \\dir e
        \\29116 f
        \\2557 g
        \\62596 h.lst
        \\$ cd e
        \\$ ls
        \\584 i
        \\$ cd ..
        \\$ cd ..
        \\$ cd d
        \\$ ls
        \\4060174 j
        \\8033020 d.log
        \\5626152 d.ext
        \\7214296 k
    );
    var result = findRm(fs);
    std.debug.print("result: {d}\n", .{result});
    try testing.expect(result == 24933642);
}
