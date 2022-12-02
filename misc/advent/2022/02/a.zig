// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

const input = @embedFile("input.txt");

const ROCK: i32 = 1;
const PAPER: i32 = 2;
const SCISSORS: i32 = 3;

pub fn decode(play: u8) i32 {
    if (play == 'A' or play == 'X') {
        return ROCK;
    }
    if (play == 'B' or play == 'Y') {
        return PAPER;
    }
    if (play == 'C' or play == 'Z') {
        return SCISSORS;
    }
    unreachable;
}

pub fn score(data: []const u8) !i32 {
    var games = std.mem.split(u8, data, "\n");
    var totalScore: i32 = 0;
    while (games.next()) |game| {
        if (game.len == 0) continue;
        var parts = std.mem.split(u8, game, " ");
        var playerA = decode(parts.next().?[0]);
        var playerB = decode(parts.next().?[0]);
        totalScore += playerB;

        // 3 points for a win, 1 for a draw
        if (playerB == playerA + 1 or playerB == playerA - 2) {
            totalScore += 6;
        } else if (playerB == playerA) {
            totalScore += 3;
        }
    }

    return totalScore;
}

pub fn main() !void {
    var s = try score(input);

    std.debug.print("part 1: {}\n", .{s});
}

// zig test a.zig
test "sample" {
    var result = try score(
        \\A Y
        \\B X
        \\C Z
    );
    try testing.expect(result == 15);
}
