// To run:
// zig run a.zig -- input.txt
//
// To compile and run:
// zig build-exe ./a.zig -O ReleaseSmall
// ./a input.txt
const std = @import("std");

const stdout = std.io.getStdOut().writer();
const stdin = std.io.getStdIn().reader();

const Instruction = enum(u8) {
    ADD = 1,
    MUL = 2,
    HALT = 99,
};
const InstructionList = std.ArrayList(i32);

const InstructionError = error{
    InvalidInstruction,
};

const BoundsError = error{
    OutOfBounds,
};

fn parse(input: []const u8, alloc: std.mem.Allocator) !InstructionList {
    var ns = InstructionList.init(alloc);

    var trimmed = std.mem.trim(u8, input, "\n");
    var nums = std.mem.split(u8, trimmed, ",");

    while (nums.next()) |n| {
        try ns.append(try std.fmt.parseInt(i32, n, 10));
    }

    return ns;
}

fn prmem(instrs: InstructionList) !void {
    var row: usize = 0;
    var width: usize = 15;
    mainloop: while (row * width < instrs.items.len) : (row += 1) {
        var col: usize = 0;
        while (col < width) : (col += 1) {
            if (row * width + col >= instrs.items.len) {
                try stdout.print("breaking\n", .{});
                break :mainloop;
            }
            try stdout.print("{d:>5}", .{instrs.items[row * width + col]});
        }
        try stdout.print("\n", .{});
    }
}

fn wait() !void {
    var line_buf: [20]u8 = undefined;
    _ = try stdin.read(&line_buf);
}

fn execute(instrs: InstructionList, debug: bool) !InstructionList {
    var ptr: usize = 0;
    while (true) {
        var instr = instrs.items[ptr];
        var mem = instrs.items;
        switch (instr) {
            @enumToInt(Instruction.ADD) => {
                var a = mem[@intCast(usize, mem[ptr + 1])];
                var b = mem[@intCast(usize, mem[ptr + 2])];
                if (debug) {
                    try stdout.print("ADD {} + {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }
                mem[@intCast(usize, mem[ptr + 3])] = a + b;
                ptr += 4;
            },
            @enumToInt(Instruction.MUL) => {
                var a = mem[@intCast(usize, mem[ptr + 1])];
                var b = mem[@intCast(usize, mem[ptr + 2])];
                if (debug) {
                    try stdout.print("MUL {} + {} -> {}\n", .{ a, b, mem[ptr + 3] });
                }
                mem[@intCast(usize, mem[ptr + 3])] = a * b;
                ptr += 4;
            },
            @enumToInt(Instruction.HALT) => {
                if (debug) {
                    try stdout.print("HALT\n", .{});
                }
                break;
            },
            else => {
                try stdout.print("Invalid instruction:  {} (-> {})\n", .{ instr, ptr });
                return InstructionError.InvalidInstruction;
            },
        }
        // TODO: add a command-line switch to enable this?
        // try prmem(instrs);
        // try wait();
    }

    return instrs;
}

test "execute" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();
    const allocator = arena.allocator();

    var expected = [_]i32{ 2, 0, 0, 0, 99 };
    var got = try execute(try parse("1,0,0,0,99", allocator), false);
    try std.testing.expectEqualSlices(i32, got.items, &expected);

    // TODO: neaten the tests up... I tried to pull this out into a function
    // and failed
    var expected2 = [_]i32{ 30, 1, 1, 4, 2, 5, 6, 0, 99 };
    got = try execute(try parse("1,1,1,4,99,5,6,0,99", allocator), false);
    try std.testing.expectEqualSlices(i32, got.items, &expected2);
}

pub fn main() !void {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const allocator = arena.allocator();

    const args = try std.process.argsAlloc(allocator);
    const input = try std.fs.cwd().readFileAlloc(allocator, args[1], std.math.maxInt(usize));
    defer allocator.free(input);

    var ns = try parse(input, allocator);
    defer ns.deinit();

    // before running the program, replace position 1 with the value 12 and
    // replace position 2 with the value 2. What value is left at position 0
    // after the program halts?
    ns.items[1] = 12;
    ns.items[2] = 2;

    _ = try execute(ns, false);

    try stdout.print("-> {}\n", .{ns.items[0]});

    // you need to determine what pair of inputs produces the output 19690720.
    // The inputs should still be provided to the program by replacing the
    // values at addresses 1 and 2, just like before. In this program, the
    // value placed in address 1 is called the noun, and the value placed in
    // address 2 is called the verb. Each of the two input values will be
    // between 0 and 99, inclusive.
    var noun: i32 = 0;
    var verb: i32 = 0;
    while (noun < 100) : (noun += 1) {
        while (verb < 100) : (verb += 1) {
            // I couldn't figure out how to copy an arraylist, so this will do
            // for now
            ns = try parse(input, allocator);
            ns.items[1] = noun;
            ns.items[2] = verb;
            _ = try execute(ns, false);
            if (ns.items[0] == 19690720) {
                try stdout.print("-> 100 * {} + {} = {}\n", .{ noun, verb, 100 * noun + verb });
                break;
            } else {
                //try stdout.print("-> {}, {} = {}\n", .{ noun, verb, ns.items[0] });
            }
        }
        verb = 0;
    }
}
