const std = @import("std");
const fs = std.fs;

const Op = enum {
    nop,
    jmp,
    acc,
};

const Instruction = struct {
    op: Op,
    val: isize,
};

const InstructionList = std.ArrayList(*Instruction);

pub fn parse(input: []const u8, alloc: *std.mem.Allocator) InstructionList {
    var instructions = InstructionList.init(alloc);
    var data = std.mem.tokenize(input, " \n");
    while (data.next()) |instruction| {
        const instr = alloc.create(Instruction) catch unreachable;
        var op = Op.nop;
        if (std.mem.eql(u8, instruction, "jmp")) {
            op = Op.jmp;
        } else if (std.mem.eql(u8, instruction, "acc")) {
            op = Op.acc;
        }
        var val = std.fmt.parseInt(isize, data.next().?, 10) catch unreachable;
        instr.* = .{
            .op = op,
            .val = val,
        };
        instructions.append(instr) catch unreachable;
    }
    return instructions;
}

test "parse" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var instr = parse("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6", &alloc.allocator);
    std.debug.print("{}\n", .{instr});
    std.testing.expect(instr.items[0].op == Op.nop);
    std.testing.expect(instr.items[0].val == 0);
    std.testing.expect(instr.items[4].op == Op.jmp);
    std.testing.expect(instr.items[4].val == -3);
}

const DupeMap = std.AutoHashMap(isize, bool);

pub fn runToDupe(instructions: InstructionList, alloc: *std.mem.Allocator) isize {
    var accum: isize = 0;
    var iptr: isize = 0;
    var dupes: DupeMap = DupeMap.init(alloc);
    defer dupes.deinit();

    while (true) {
        const dupe = dupes.getOrPut(iptr) catch unreachable;
        if (dupe.found_existing) {
            std.debug.print("found existing key {}\n", .{iptr});
            break;
        }

        var instr = instructions.items[@intCast(usize, iptr)];
        switch (instr.op) {
            Op.nop => iptr += 1,
            Op.acc => {
                accum += instr.val;
                iptr += 1;
            },
            Op.jmp => iptr += instr.val,
        }
    }
    return accum;
}

test "runToDupe" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var instr = parse("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6", &alloc.allocator);
    var a = runToDupe(instr, &alloc.allocator);
    std.testing.expect(a == 5);
}

pub fn runToEnd(instructions: InstructionList) ?isize {
    var accum: isize = 0;
    var iptr: isize = 0;
    var counter: usize = 0;

    while (iptr != instructions.items.len) {
        counter += 1;
        if (counter > 10000) {
            return null;
        }

        if (iptr < 0 or iptr > instructions.items.len) {
            std.debug.print("overflow {}\n", .{iptr});
            return null;
        }

        var instr = instructions.items[@intCast(usize, iptr)];
        switch (instr.op) {
            Op.nop => iptr += 1,
            Op.acc => {
                accum += instr.val;
                iptr += 1;
            },
            Op.jmp => iptr += instr.val,
        }
    }

    return accum;
}

pub fn flip(instruction: *Instruction) void {
    if (instruction.op == Op.jmp) {
        instruction.op = Op.nop;
    } else {
        instruction.op = Op.jmp;
    }
}

pub fn findFix(instructions: InstructionList) isize {
    var i: usize = 0;

    while (i < instructions.items.len) : (i += 1) {
        if (instructions.items[i].op != Op.jmp and
            instructions.items[i].op != Op.nop)
        {
            continue;
        }
        flip(instructions.items[i]);
        var accum = runToEnd(instructions);
        if (accum != null) {
            std.debug.print("successfully flipped {}\n", .{i});
            return accum.?;
        }
        flip(instructions.items[i]);
    }
    // should never get here
    return -1;
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var args = try std.process.argsAlloc(&alloc.allocator);
    const input = try std.fs.cwd().readFileAlloc(&alloc.allocator, args[1], std.math.maxInt(usize));

    var instructions = parse(input, &alloc.allocator);
    var accum = runToDupe(instructions, &alloc.allocator);
    std.debug.print("accumulator {}\n", .{accum});
    accum = findFix(instructions);
    std.debug.print("accumulator {}\n", .{accum});
}
