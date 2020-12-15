const std = @import("std");
const fs = std.fs;

const LastSaid = std.AutoHashMap(usize, usize);

pub fn solve(inp: []usize, stop: usize, alloc: *std.mem.Allocator) usize {
    var said = LastSaid.init(alloc);
    defer said.deinit();

    // factor of 64 determined empirically
    said.ensureCapacity(@intCast(u32, stop / 64)) catch unreachable;

    for (inp[0 .. inp.len - 1]) |elt, i| {
        said.put(elt, i + 1) catch unreachable;
    }

    var turn: usize = inp.len + 1;
    var current: usize = inp[inp.len - 1];
    while (true) {
        var res = said.getOrPut(current) catch unreachable;
        if (res.found_existing) {
            current = (turn - 1) - res.entry.value;
        } else {
            current = 0;
        }
        res.entry.value = turn - 1;
        turn += 1;
        if (turn > stop) {
            return current;
        }
    }
}

test "solve" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var inp = [_]usize{ 0, 3, 6 };
    std.testing.expect(solve(inp[0..], 2020, &alloc.allocator) == 436);
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var inp = [_]usize{ 1, 0, 18, 10, 19, 6 };
    std.debug.print("{}\n", .{solve(inp[0..], 2020, &alloc.allocator)});
    std.debug.print("{}\n", .{solve(inp[0..], 30_000_000, &alloc.allocator)});
}
