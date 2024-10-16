// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

const input = @embedFile("input.txt");

const ROCK: i32 = 1;
const PAPER: i32 = 2;
const SCISSORS: i32 = 3;

pub fn decode(play: u8) i32 {
    return switch (play) {
        'A', 'X' => ROCK,
        'B', 'Y' => PAPER,
        'C', 'Z' => SCISSORS,
        else => unreachable,
    };
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

        // 6 points for a win, 3 for a draw
        if (playerB == playerA + 1 or playerB == playerA - 2) {
            totalScore += 6;
        } else if (playerB == playerA) {
            totalScore += 3;
        }
    }

    return totalScore;
}

const LOSE = 'X';
const DRAW = 'Y';
const WIN = 'Z';

pub fn choosePlay(opponent: i32, play: u8) i32 {
    if (play == DRAW) {
        return opponent;
    } else if (play == LOSE) {
        return switch (opponent) {
            ROCK => SCISSORS,
            PAPER => ROCK,
            SCISSORS => PAPER,
            else => unreachable,
        };
    } else {
        return switch (opponent) {
            ROCK => PAPER,
            PAPER => SCISSORS,
            SCISSORS => ROCK,
            else => unreachable,
        };
    }
}

pub fn score2(data: []const u8) !i32 {
    var games = std.mem.split(u8, data, "\n");
    var totalScore: i32 = 0;
    while (games.next()) |game| {
        if (game.len == 0) continue;
        var parts = std.mem.split(u8, game, " ");
        var playerA = decode(parts.next().?[0]);
        var playerB = choosePlay(playerA, parts.next().?[0]);
        totalScore += playerB;

        // 6 points for a win, 3 for a draw
        if (playerB == playerA + 1 or playerB == playerA - 2) {
            totalScore += 6;
        } else if (playerB == playerA) {
            totalScore += 3;
        }
    }

    return totalScore;
}

pub fn main() !void {
    std.debug.print("part 1: {}\n", .{try score(input)});
    std.debug.print("part 2: {}\n", .{try score2(input)});
}

// zig test a.zig
test "part 1" {
    var result = try score(
        \\A Y
        \\B X
        \\C Z
    );
    try testing.expect(result == 15);
}

test "part 2" {
    var result = try score2(
        \\A Y
        \\B X
        \\C Z
    );
    try testing.expect(result == 12);
}
