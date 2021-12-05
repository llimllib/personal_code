// incomplete zig answer for day 4, trying to remember how to do stuff
const std = @import("std");

const Board = [25]i32;

const Bingo = struct {
    numbers: std.ArrayList(i32),
    boards: std.ArrayList(Board),
};

const OUT = std.io.getStdOut().writer();

// stole a bunch from https://zigbin.io/ad5582
fn parse(input: []const u8, alloc: std.mem.Allocator) !Bingo {
    var bingo = Bingo{
        .numbers = std.ArrayList(i32).init(alloc),
        .boards = std.ArrayList(Board).init(alloc),
    };
    var trimmed = std.mem.trim(u8, input, "\n");
    var data = std.mem.split(u8, trimmed, "\n\n");
    var ns = std.mem.split(u8, data.next().?, ",");
    while (ns.next()) |n| {
        try bingo.numbers.append(try std.fmt.parseInt(i32, n, 10));
    }
    while (data.next()) |board_data| {
        var board = try bingo.boards.addOne();
        var bns = std.mem.tokenize(u8, board_data, &std.ascii.spaces);
        var boardi: usize = 0;
        while (bns.next()) |bn| : (boardi += 1) {
            board[boardi] = try std.fmt.parseInt(i32, bn, 10);
        }
        std.debug.assert(boardi == 25);
    }

    return bingo;
}

fn run(bingo: Bingo) !void {
    for (bingo.numbers.items) |n| {
        try OUT.print("{d}\n", .{n});
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    const bingo = try parse(input, allocator);
    try run(bingo);
}
