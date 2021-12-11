// To run:
// zig run a.zig -- input.txt [input vars...]
//
// To compile and run:
// zig build-exe ./a.zig -O ReleaseSmall
// ./a input.txt [input vars...]
//
// I tried debugging with:
// zig run a.zig --test-cmd lldb --test-cmd-bin -- small.txt
//
// and was able to set a breakpoint:
//
// (lldb) break set -f a.zig -l 45
// Breakpoint 1: where = a`flash + 24 at a.zig:46:15, address = 0x0000000100009a88
//
// but then the process segfaulted when I did "run"
const std = @import("std");

const stdout = std.io.getStdOut().writer();

const FLASH = 9;
const FLASHED = 100;

const width: usize = 10;
var board: [width][width]u8 = undefined;

fn parse(input: []const u8) void {
    var trimmed = std.mem.trim(u8, input, "\n");
    var lines = std.mem.split(u8, trimmed, "\n");

    var row: usize = 0;
    while (lines.next()) |line| : (row += 1) {
        var col: usize = 0;
        while (col < width) : (col += 1) {
            board[row][col] = std.fmt.parseInt(u8, line[col .. col + 1], 10) catch unreachable;
        }
    }
}

fn coredump() void {
    var row: usize = 0;
    while (row < width) : (row += 1) {
        var col: usize = 0;
        while (col < width) : (col += 1) {
            if (board[row][col] <= FLASH) {
                stdout.print("{}", .{board[row][col]}) catch unreachable;
            } else {
                stdout.print("\u{001b}[31m0\u{001b}[0m", .{}) catch unreachable;
            }
        }
        stdout.print("\n", .{}) catch unreachable;
    }
}

fn flash(row: usize, col: usize) void {
    if (board[row][col] == FLASH) {
        board[row][col] = FLASHED;
        if (row > 0 and col > 0) {
            board[row - 1][col - 1] += 1;
            flash(row - 1, col - 1);
        }
        if (row > 0) {
            board[row - 1][col] += 1;
            flash(row - 1, col);
        }
        if (row > 0 and col + 1 < width) {
            board[row - 1][col + 1] += 1;
            flash(row - 1, col + 1);
        }
        if (col + 1 < width) {
            board[row][col + 1] += 1;
            flash(row, col + 1);
        }
        if (row + 1 < width and col + 1 < width) {
            board[row + 1][col + 1] += 1;
            flash(row + 1, col + 1);
        }
        if (row + 1 < width) {
            board[row + 1][col] += 1;
            flash(row + 1, col);
        }
        if (row + 1 < width and col > 0) {
            board[row + 1][col - 1] += 1;
            flash(row + 1, col - 1);
        }
        if (col > 0) {
            board[row][col - 1] += 1;
            flash(row, col - 1);
        }
    }
}

fn run(steps: usize) void {
    var i: usize = 0;

    // First, the energy level of each octopus increases by 1.
    while (i < steps) : (i += 1) {
        var row: usize = 0;
        while (row < width) : (row += 1) {
            var col: usize = 0;
            while (col < width) : (col += 1) {
                // if the spot flashed last round, it goes to 1
                if (board[row][col] > FLASH) {
                    board[row][col] = 1;
                } else {
                    board[row][col] += 1;
                }
            }
        }
        // Then, any octopus with an energy level greater than 9 flashes. This
        // increases the energy level of all adjacent octopuses by 1, including
        // octopuses that are diagonally adjacent. If this causes an octopus to
        // have an energy level greater than 9, it also flashes. This process
        // continues as long as new octopuses keep having their energy level
        // increased beyond 9. (An octopus can only flash at most once per step.)
        row = 0;
        while (row < width) : (row += 1) {
            var col: usize = 0;
            while (col < width) : (col += 1) {
                if (board[row][col] > FLASH) {
                    flash(row, col);
                }
            }
        }
    }
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    // const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, "small.txt", std.math.maxInt(usize));
    defer allocator.free(input);

    parse(input);
    coredump();
    stdout.print("------------\n", .{}) catch unreachable;
    run(1);
    coredump();
    stdout.print("------------\n", .{}) catch unreachable;
    run(1);
    coredump();
    stdout.print("------------\n", .{}) catch unreachable;
}
