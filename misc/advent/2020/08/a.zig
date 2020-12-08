const std = @import("std");
const fs = std.fs;

const InstructionList = std.ArrayList(*Instruction);

const Instruction = struct {
    instr: []const u8,
    val: isize,
};

pub fn parse(input: []const u8, alloc: *std.mem.Allocator) InstructionList {
    var instructions = InstructionList.init(alloc);
    var data = std.mem.tokenize(input, " \n");
    while (data.next()) |instruction| {
        const instr = alloc.create(Instruction) catch unreachable;
        var val = std.fmt.parseInt(isize, data.next().?, 10) catch unreachable;
        instr.* = .{
            .instr = instruction,
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
    std.testing.expect(std.mem.eql(u8, instr.items[0].instr, "nop"));
    std.testing.expect(instr.items[0].val == 0);
    std.testing.expect(std.mem.eql(u8, instr.items[4].instr, "jmp"));
    std.testing.expect(instr.items[4].val == -3);
}

const DupeMap = std.AutoHashMap(isize, bool);

test "DupeMap" {
    var map = DupeMap.init(std.testing.allocator);
    defer map.deinit();

    var iptr: isize = 0;

    const gop = try map.getOrPut(iptr);
    std.testing.expect(gop.found_existing == false);
    gop.entry.value = true;
    std.testing.expect(map.getEntry(iptr).?.value == true);

    const gop2 = try map.getOrPut(iptr);
    std.testing.expect(gop2.found_existing == true);
}

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
        if (std.mem.eql(u8, instr.instr, "nop")) {
            iptr += 1;
        } else if (std.mem.eql(u8, instr.instr, "acc")) {
            accum += instr.val;
            iptr += 1;
        } else if (std.mem.eql(u8, instr.instr, "jmp")) {
            iptr += instr.val;
        }
    }
    return accum;
}

test "runToDupe" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var instr = parse("nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6", &alloc.allocator);
    var a = runToDupe(instr, &alloc.allocator);
    std.debug.print("{}\n", .{a});
}

pub fn runToEnd(instructions: InstructionList, alloc: *std.mem.Allocator) ?isize {
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
        // XXX switch
        if (std.mem.eql(u8, instr.instr, "nop")) {
            iptr += 1;
        } else if (std.mem.eql(u8, instr.instr, "acc")) {
            accum += instr.val;
            iptr += 1;
        } else if (std.mem.eql(u8, instr.instr, "jmp")) {
            iptr += instr.val;
        }
    }

    return accum;
}

pub fn flip(instruction: *Instruction) void {
    if (std.mem.eql(u8, instruction.instr, "jmp")) {
        instruction.instr = "nop";
    } else {
        instruction.instr = "jmp";
    }
}

pub fn findFix(instructions: InstructionList, alloc: *std.mem.Allocator) isize {
    var i: usize = 0;

    done: while (i < instructions.items.len) : (i += 1) {
        if (!std.mem.eql(u8, instructions.items[i].instr, "jmp") and
            !std.mem.eql(u8, instructions.items[i].instr, "nop"))
        {
            continue;
        }
        flip(instructions.items[i]);
        var accum = runToEnd(instructions, alloc);
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
    accum = findFix(instructions, &alloc.allocator);
    std.debug.print("accumulator {}\n", .{accum});
}
