// To run:
// zig run a.zig -- input.txt [input vars...]
//
// To compile and run:
// zig build-exe ./a.zig -O ReleaseSmall
// ./a input.txt [input vars...]
//
// to debug with lldb:
// zig run a.zig --test-cmd lldb --test-cmd-bin -- small.txt
// (lldb) break set -name flash
// Breakpoint 1: where = a`flash + 24 at a.zig:46:15, address = 0x0000000100009a88
// (lldb) run
const std = @import("std");

const stdout = std.io.getStdOut().writer();

const Node = struct {
    name: []const u8,
    edges: NodeList,
    seen: bool,
    big: bool,
};

const NodeList = std.ArrayList(*Node);

// node name -> Node
const NodeIndex = std.StringHashMap(*Node);

var index: NodeIndex = undefined;

fn log(comptime format: []const u8, args: anytype) void {
    stdout.print(format, args) catch unreachable;
}

fn parse(alloc: std.mem.Allocator, input: []const u8) *Node {
    var trimmed = std.mem.trim(u8, input, "\n");
    var lines = std.mem.split(u8, trimmed, "\n");
    index = NodeIndex.init(alloc);

    var root = Node{
        .name = "start",
        .edges = NodeList.init(alloc),
        .seen = false,
        .big = false,
    };
    index.put("start", &root) catch unreachable;

    while (lines.next()) |line| {
        var nodes = std.mem.split(u8, line, "-");
        var a = nodes.next().?;
        var b = nodes.next().?;

        // lib/std/mem/alloc.zig:/// Copies `m` to newly allocated memory. Caller owns the memory.
        // lib/std/mem/alloc.zig-pub fn dupe(alloc: alloc, comptime T: type, m: []const T) ![]T {
        // lib/std/mem/alloc.zig-    const new_buf = try alloc.alloc(T, m.len);
        // lib/std/mem/alloc.zig-    mem.copy(T, new_buf, m);
        // lib/std/mem/alloc.zig-    return new_buf;
        // lib/std/mem/alloc.zig-}
        if (!index.contains(a))
            index.put(a, &Node{
                .name = alloc.dupe(u8, a) catch unreachable,
                .edges = NodeList.init(alloc),
                .seen = false,
                .big = std.ascii.isUpper(a[0]),
            }) catch unreachable;
        if (!index.contains(b))
            index.put(b, &Node{
                .name = alloc.dupe(u8, b) catch unreachable,
                .edges = NodeList.init(alloc),
                .seen = false,
                .big = std.ascii.isUpper(a[0]),
            }) catch unreachable;

        index.get(a).?.edges.append(&index.get(b).?) catch unreachable;
        index.get(b).?.edges.append(&index.get(a).?) catch unreachable;
    }

    return &root;
}

test "parse" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const alloc = arena.allocator();

    var input = try std.fs.cwd().readFileAlloc(alloc, "small.txt", std.math.maxInt(usize));
    defer alloc.free(input);

    var root = parse(alloc, input);
    try std.testing.expect(std.mem.eql(u8, root.name, "start"));
    try std.testing.expect(root.edges.items.len == 2);
    try std.testing.expect(std.mem.eql(u8, root.edges.items[0], "A"));
    try std.testing.expect(std.mem.eql(u8, root.edges.items[1], "b"));
    for (root.edges.items) |node| {
        log("{s}\n", .{node});
    }
}

fn paths(alloc: std.mem.Allocator, root: *Node) i32 {
    log("--- {s}\n", .{root});
    var frontier: std.ArrayList(*Node) = std.ArrayList(*Node).init(alloc);
    frontier.append(root) catch unreachable;

    var n: i32 = 0;
    while (frontier.items.len > 0) {
        var node = frontier.pop();
        node.seen = true;
        n += 1;
        log(">>>{s}<\n", .{node});
        for (node.edges.items) |edge_name| {
            log("{s}\n", .{edge_name});
            var edge = index.get(edge_name).?;
            if (!edge.seen or edge.big)
                frontier.append(edge) catch unreachable;
        }
    }

    return n;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const alloc = arena.allocator();

    const args = try std.process.argsAlloc(alloc);
    var input = try std.fs.cwd().readFileAlloc(alloc, args[1], std.math.maxInt(usize));
    defer alloc.free(input);

    log("{}\n", .{paths(alloc, parse(alloc, input))});
}
