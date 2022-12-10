// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const InstructionType = enum {
    NOOP,
    ADDX,
};

const Instruction = struct {
    type: InstructionType,
    arg1: i64,
};

const InstructionList = std.ArrayList(*Instruction);

const CPU = struct {
    registers: []i64,
    tick: u64,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) CPU {
        var registers = alloc.alloc(i64, 1) catch unreachable;
        registers[0] = 1;
        return CPU{
            .registers = registers,
            .tick = 0,
            .alloc = alloc,
        };
    }

    pub fn run(self: *CPU, instrs: InstructionList) i64 {
        self.tick = 0;
        var lock = false;
        var iptr: usize = 0;
        var signalStrength: i64 = 0;
        while (true) {
            var instr = instrs.items[iptr];
            self.tick += 1;

            if (self.tick == 20 or self.tick == 60 or self.tick == 100 or
                self.tick == 140 or self.tick == 180 or self.tick == 220)
            {
                signalStrength += @intCast(i64, self.tick) * self.registers[0];
            }

            switch (instr.type) {
                .NOOP => {},
                .ADDX => {
                    if (!lock) {
                        lock = true;
                        continue;
                    }
                    lock = false;
                    self.registers[0] += instr.arg1;
                },
            }

            iptr += 1;
            if (iptr >= instrs.items.len) {
                break;
            }
        }

        return signalStrength;
    }
};

pub fn parse(data: []const u8) InstructionList {
    var list = InstructionList.init(gpa);
    var lines = std.mem.split(u8, data, "\n");
    while (lines.next()) |line| {
        var instr = gpa.create(Instruction) catch unreachable;
        list.append(instr) catch unreachable;

        var parts = std.mem.split(u8, line, " ");
        var cmd = parts.next().?;
        if (std.mem.eql(u8, cmd, "noop")) {
            instr.* = .{ .type = .NOOP, .arg1 = 0 };
        } else if (std.mem.eql(u8, cmd, "addx")) {
            var value = std.fmt.parseInt(i64, parts.next().?, 10) catch unreachable;
            instr.* = .{ .type = .ADDX, .arg1 = value };
        }
    }
    return list;
}

pub fn main() !void {
    var instructions = parse(input);
    var cpu = CPU.init(gpa);
    var signal = cpu.run(instructions);
    std.debug.print("signal: {d}\n", .{signal});
}

// zig test a.zig
test "part 1" {
    const sample = @embedFile("sample.txt");
    var result = parse(sample);
    std.debug.print("{any}\n", .{result.items[0]});
    try testing.expect(result.items[0].arg1 == 15);

    var cpu = CPU.init(gpa);
    var signal = cpu.run(result);
    std.debug.print("{d}\n", .{signal});
    try testing.expect(signal == 13140);
}
