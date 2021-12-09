// To run:
// zig run a.zig -- input.txt [input vars...]
//
// To compile and run:
// zig build-exe ./a.zig -O ReleaseSmall
// ./a input.txt [input vars...]
const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();
const pow = std.math.pow;

const Op = enum(u8) {
    ADD = 1,
    MUL = 2,
    INPUT = 3,
    OUTPUT = 4,
    JIT = 5,
    JIF = 6,
    LT = 7,
    EQ = 8,
    HALT = 99,
};
const Instruction = struct {
    op: Op,
    flags: u8,
};

const OpError = error{
    InvalidOp,
    InvalidParam,
};

const Mode = enum(u8) {
    POSITION = 0,
    IMMEDIATE = 1,
};

// 64k ought to be enough for anybody
var mem: [64 * 1024]i64 = undefined;
var mem_len: usize = 0;

fn parse(allocator: std.mem.Allocator, input: []const u8, input_buf: []const u8) !std.ArrayList(i64) {
    var trimmed = std.mem.trim(u8, input, "\n");
    var nums = std.mem.split(u8, trimmed, ",");

    var i: usize = 0;
    while (nums.next()) |n| : (i += 1) {
        mem[i] = try std.fmt.parseInt(i64, n, 10);
    }
    mem_len = i;

    var input_lst = std.ArrayList(i64).init(allocator);
    var inputs = std.mem.split(u8, input_buf, " ");
    while (inputs.next()) |in| {
        try input_lst.insert(0, try std.fmt.parseInt(i64, in, 10));
    }
    return input_lst;
}

// Pause and wait for user input
fn wait() !void {
    var line_buf: [20]u8 = undefined;
    _ = try stdin.read(&line_buf);
}

fn coredump() void {
    var row: usize = 0;
    var width: usize = 20;
    while (row * width < mem_len) : (row += 1) {
        var col: usize = 0;
        while (col < width) : (col += 1) {
            stdout.print(" {} ", .{mem[(row * width) + col]}) catch unreachable;
        }
        stdout.print("\n", .{}) catch unreachable;
    }
}

fn param(op: i64, n: i64) OpError!Mode {
    return std.meta.intToEnum(Mode, @mod(@divFloor(op, pow(i64, 10, n + 1)), 10)) catch {
        stdout.print("Invalid param {} {}\n", .{ op, n }) catch unreachable;
        coredump();
        return OpError.InvalidParam;
    };
}

// ref: The opcode is a two-digit number based only on the ones and tens digit
// of the value, that is, the opcode is the rightmost two digits of the first
// value in an instruction.
fn opcode(n: i64) OpError!Op {
    return std.meta.intToEnum(Op, @mod(n, 100)) catch {
        stdout.print("Invalid op {}\n", .{n}) catch unreachable;
        coredump();
        return OpError.InvalidOp;
    };
}

fn execute(inputs: *std.ArrayList(i64), debug: bool) !void {
    var ptr: usize = 0;
    while (true) {
        var instr = mem[ptr];
        switch (try opcode(instr)) {
            Op.ADD => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("ADD {} + {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }
                mem[@intCast(usize, mem[ptr + 3])] = a + b;
                ptr += 4;
            },
            Op.MUL => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("MUL {} + {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }
                mem[@intCast(usize, mem[ptr + 3])] = a * b;
                ptr += 4;
            },
            Op.JIT => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("JIT {} {}\n", .{ a, b });
                }

                ptr += 3;
                if (a != 0) {
                    ptr = @intCast(usize, b);
                }
            },
            Op.JIF => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("JIF {} {}\n", .{ a, b });
                }

                ptr += 3;
                if (a == 0) {
                    ptr = @intCast(usize, b);
                }
            },
            Op.LT => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("LT {} < {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }

                if (a < b) {
                    mem[@intCast(usize, mem[ptr + 3])] = 1;
                } else {
                    mem[@intCast(usize, mem[ptr + 3])] = 0;
                }
                ptr += 4;
            },
            Op.EQ => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                var b = mem[ptr + 2];
                if ((try param(instr, 2)) == Mode.POSITION)
                    b = mem[@intCast(usize, b)];

                if (debug) {
                    try stdout.print("EQ {} == {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }

                if (a == b) {
                    mem[@intCast(usize, mem[ptr + 3])] = 1;
                } else {
                    mem[@intCast(usize, mem[ptr + 3])] = 0;
                }
                ptr += 4;
            },
            Op.INPUT => {
                var a = mem[ptr + 1];

                // XXX in the python I read from a static buffer provided to
                // the `execute` function - do that?
                // try stdout.print("input: ", .{});
                // var line_buf: [20]u8 = undefined;
                // const amt = try stdin.read(&line_buf);
                // const line = std.mem.trimRight(u8, line_buf[0..amt], "\r\n");
                // const input = try std.fmt.parseUnsigned(u8, line, 10);
                var input = inputs.pop();

                if (debug) {
                    try stdout.print("STORE {} -> {}\n", .{ input, a });
                }

                mem[@intCast(usize, a)] = input;
                ptr += 2;
            },
            Op.OUTPUT => {
                var a = mem[ptr + 1];
                if ((try param(instr, 1)) == Mode.POSITION)
                    a = mem[@intCast(usize, a)];

                if (debug) {
                    try stdout.print("OUTPUT {}\n", .{a});
                }
                try stdout.print("{}\n", .{a});
                ptr += 2;
            },
            Op.HALT => {
                if (debug) {
                    try stdout.print("HALT\n", .{});
                }
                break;
            },
        }
    }
}

test "execute" {
    try parse("1,0,0,0,99");
    try execute(false);

    // TODO: neaten the tests up... I tried to pull this out into a function
    // and failed
    // TOOD: rewrite tests for mem model
    // var expected2 = [_]i32{ 30, 1, 1, 4, 2, 5, 6, 0, 99 };
    // try execute(try parse("1,1,1,4,99,5,6,0,99", allocator), false);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    var inputs = try parse(allocator, input, args[2]);
    defer inputs.deinit();

    try execute(&inputs, false);
}
