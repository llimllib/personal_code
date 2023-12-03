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
    INPUT = 1,
    ADD = 2,
    MUL = 3,
    DIV = 4,
    MOD = 5,
    EQ = 6,
};
const Instruction = struct {
    op: Op,
    x: []const u8,
    y: []const u8,
};
const InstructionList = std.ArrayList(Instruction);

var mem: [4]i64 = undefined;
var cmds: InstructionList;

fn parse(alloc: std.mem.Allocator, input: []const u8) void {
    var trimmed = std.mem.trim(u8, input, "\n");
    var cmds = std.mem.split(u8, trimmed, "\n");

    while (cmds.next()) |cmd| : (i += 1) {
        var op = InstructionList.addOne();
        var parts = std.mem.split(cmd, cmd, " ");
        switch (parts.next()) |op| {
            case "inp":
                op.op = Op.INPUT;
                const partx = parts.next();
                const x = try alloc.alloc(u8, partx.len);
                op.x = std.mem.copy(u8, x, partx);
        }
        std.fmt.parseInt(i64, n, 10) catch unreachable;
    }
    mem_len = i;
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

fn getparam(loc: usize, op: i64, n: usize, relative_base: i64) i64 {
    var p = mem[loc + n];
    return switch (param(op, @intCast(i64, n)) catch unreachable) {
        Mode.POSITION => mem[@intCast(usize, p)],
        Mode.RELATIVE => mem[@intCast(usize, relative_base + p)],
        Mode.IMMEDIATE => p,
    };
}

fn outparam(loc: usize, op: i64, n: usize, relative_base: i64) usize {
    var p = mem[loc + n];
    return @intCast(usize, switch (param(op, @intCast(i64, n)) catch unreachable) {
        Mode.POSITION => p,
        Mode.RELATIVE => relative_base + p,
        // Parameters that an instruction writes to will never be in immediate mode
        // https://adventofcode.com/2019/day/5#part2
        Mode.IMMEDIATE => unreachable,
    });
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

fn execute(debug: bool) !void {
    var opcount: i64 = 0;
    var ptr: usize = 0;

    // relative mode parameters use this as their offset. changed by RBO
    // operation
    var relative_base: i64 = 0;

    var start = std.time.nanoTimestamp();

    while (true) : (opcount += 1) {
        var instr = mem[ptr];
        switch (try opcode(instr)) {
            Op.ADD => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);
                var c = outparam(ptr, instr, 3, relative_base);

                if (debug)
                    try stdout.print("{:5} ADD {} + {} -> {}\n", .{ instr, a, b, mem[ptr + 3] });

                mem[c] = a + b;
                ptr += 4;
            },
            Op.MUL => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);
                var c = outparam(ptr, instr, 3, relative_base);

                if (debug)
                    try stdout.print("{:5} MUL {} + {} -> {}\n", .{ instr, a, b, mem[ptr + 3] });

                mem[c] = a * b;
                ptr += 4;
            },
            Op.JIT => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);

                if (debug)
                    try stdout.print("{:5} JIT {} {}\n", .{ instr, a, b });

                ptr += 3;
                if (a != 0)
                    ptr = @intCast(usize, b);
            },
            Op.JIF => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);

                if (debug)
                    try stdout.print("{:5} JIF {} {}\n", .{ instr, a, b });

                ptr += 3;
                if (a == 0)
                    ptr = @intCast(usize, b);
            },
            Op.LT => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);
                var c = outparam(ptr, instr, 3, relative_base);

                if (debug)
                    try stdout.print("{:5} LT {} < {} -> {}\n", .{ instr, a, b, mem[ptr + 3] });

                mem[c] = if (a < b) 1 else 0;
                ptr += 4;
            },
            Op.EQ => {
                var a = getparam(ptr, instr, 1, relative_base);
                var b = getparam(ptr, instr, 2, relative_base);
                var c = outparam(ptr, instr, 3, relative_base);

                if (debug)
                    try stdout.print("{:5} EQ {} == {} -> {}\n", .{ instr, a, b, mem[ptr + 3] });

                mem[c] = if (a == b) 1 else 0;
                ptr += 4;
            },
            Op.INPUT => {
                // we're not properly handling relative base here, we're getting a 203 but get 3 back instead of 1000
                var a = outparam(ptr, instr, 1, relative_base);

                var line_buf: [20]u8 = undefined;
                const n = try stdin.read(&line_buf);
                const line = std.mem.trimRight(u8, line_buf[0..n], "\r\n");
                const input = try std.fmt.parseInt(i64, line, 10);

                if (debug)
                    try stdout.print("{:5} INPUT {} -> {}\n", .{ instr, input, a });

                mem[a] = input;
                ptr += 2;
            },
            Op.OUTPUT => {
                var a = getparam(ptr, instr, 1, relative_base);

                if (debug) {
                    try stdout.print("{:5} OUTPUT {}\n", .{ instr, a });
                }
                try stdout.print("{}\n", .{a});
                try stdout.flush();
                ptr += 2;
            },
            Op.RBO => {
                var a = getparam(ptr, instr, 1, relative_base);

                if (debug)
                    try stdout.print("{:5} RBO {} + {}\n", .{ instr, relative_base, a });

                relative_base += a;

                ptr += 2;
            },
            Op.HALT => {
                if (debug) {
                    try stdout.print("{} HALT\n", .{instr});
                }
                break;
            },
        }
    }

    var finish = std.time.nanoTimestamp();
    var dur = @intToFloat(f64, finish - start) / 1_000_000_000;
    stdout.print("run took {}s for {} ops [{:.2} ops/s]\n", .{ dur, opcount, @floatToInt(u64, (@intToFloat(f64, opcount) / dur)) }) catch unreachable;
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    parse(input);

    try execute(false);
}
