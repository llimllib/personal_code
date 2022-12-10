// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const stdin = std.io.getStdIn().reader();
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
    width: u8,
    height: u8,
    // I literally cannot figure out how to do a multidimensional array in zig,
    // so we'll just use an array and multiplication
    // I also cannot figure out how to allocate a multidimensional array, so
    // we'll use a 1d slice instead of an array
    crt: []u8,
    alloc: std.mem.Allocator,

    pub fn init(alloc: std.mem.Allocator) CPU {
        var registers = alloc.alloc(i64, 1) catch unreachable;
        registers[0] = 1;
        var width: u8 = 40;
        var height: u8 = 6;
        var crt = alloc.alloc(u8, width * height) catch unreachable;
        for (crt) |_, i| crt[i] = '.';
        return CPU{
            .registers = registers,
            .tick = 0,
            .width = width,
            .height = height,
            .crt = crt,
            .alloc = alloc,
        };
    }

    fn printCRT(self: *CPU) void {
        var row: usize = 0;
        var col: usize = 0;
        std.debug.print("\n", .{});
        while (row < self.height) : (row += 1) {
            while (col < self.width) : (col += 1) {
                std.debug.print("{c}", .{self.crt[row * self.width + col]});
            }
            col = 0;
            std.debug.print("\n", .{});
        }
        std.debug.print("-----------\n", .{});
    }

    fn updateCRT(self: *CPU) void {
        var row = ((self.tick - 1) / self.width) % self.height;
        var col = (self.tick - 1) % self.width;
        var dx = std.math.absInt(self.registers[0] - @intCast(i64, col)) catch unreachable;
        if (dx < 2) {
            self.crt[row * self.width + col] = '#';
        } else {
            self.crt[row * self.width + col] = '.';
        }
    }

    pub fn run(self: *CPU, instrs: InstructionList) i64 {
        self.tick = 0;
        var lock = false;
        var iptr: usize = 0;
        var signalStrength: i64 = 0;
        while (true) {
            var instr = instrs.items[iptr];
            self.tick += 1;

            self.updateCRT();

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

        self.printCRT();
        return signalStrength;
    }
};

pub fn parse(data: []const u8) InstructionList {
    var list = InstructionList.init(gpa);
    var lines = std.mem.split(u8, data, "\n");
    while (lines.next()) |line| {
        if (line.len == 0) continue;

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
    try testing.expect(result.items[0].arg1 == 15);

    var cpu = CPU.init(gpa);
    var signal = cpu.run(result);
    std.debug.print("{d}\n", .{signal});
    try testing.expect(signal == 13140);
}
