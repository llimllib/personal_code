// run with: zig run a.zig
const std = @import("std");
const testing = std.testing;

var gpa_impl = std.heap.GeneralPurposeAllocator(.{}){};
pub const gpa = gpa_impl.allocator();

const input = @embedFile("input.txt");

const Stack = std.ArrayList(u8);

const Input = struct {
    stacks: []Stack,
    commands: std.ArrayList([3]u8),
};

pub fn parse(data: []const u8) *Input {
    var stacks = gpa.alloc(Stack, 9) catch unreachable;
    for (stacks) |_, i| stacks[i] = Stack.init(gpa);

    var parts = std.mem.split(u8, data, "\n\n");
    var stacklines = std.mem.split(u8, parts.next().?, "\n");
    while (stacklines.next()) |line| {
        var j: usize = 0;
        while (j < 9) : (j += 1) {
            var idx = 1 + j * 4;
            if (idx < line.len and line[idx] >= 'A' and line[idx] <= 'Z') {
                stacks[j].insert(0, line[idx]) catch unreachable;
            }
        }
    }

    var commands = std.ArrayList([3]u8).init(gpa);
    var cmds = std.mem.split(u8, parts.next().?, "\n");
    while (cmds.next()) |cmd| {
        if (cmd.len == 0) continue;

        var cmdparts = std.mem.split(u8, cmd, " ");
        _ = cmdparts.next();
        var n = std.fmt.parseInt(u8, cmdparts.next().?, 10) catch unreachable;
        _ = cmdparts.next();
        var from = std.fmt.parseInt(u8, cmdparts.next().?, 10) catch unreachable;
        _ = cmdparts.next();
        var to = std.fmt.parseInt(u8, cmdparts.next().?, 10) catch unreachable;
        commands.append([3]u8{ n, from - 1, to - 1 }) catch unreachable;
    }

    var parsedInput = gpa.create(Input) catch unreachable;
    parsedInput.* = Input{
        .stacks = stacks,
        .commands = commands,
    };
    return parsedInput;
}

pub fn crateMover9000(inp: *Input) []const u8 {
    for (inp.commands.items) |command| {
        var i: usize = 0;
        while (i < command[0]) : (i += 1) {
            var it = inp.stacks[command[1]].pop();
            inp.stacks[command[2]].append(it) catch unreachable;
        }
    }

    var result: []u8 = gpa.alloc(u8, 9) catch unreachable;
    for (inp.stacks) |stack, i| {
        if (stack.items.len > 0) {
            result[i] = stack.items[stack.items.len - 1];
        } else {
            result[i] = '_';
        }
    }
    return result[0..];
}

pub fn crateMover9001(inp: *Input) []const u8 {
    for (inp.commands.items) |command| {
        var i: usize = 0;
        var tempStack = Stack.init(gpa);
        while (i < command[0]) : (i += 1) {
            var it = inp.stacks[command[1]].pop();
            tempStack.insert(0, it) catch unreachable;
        }
        var j: usize = 0;
        while (j < tempStack.items.len) : (j += 1) {
            inp.stacks[command[2]].append(tempStack.items[j]) catch unreachable;
        }
    }

    var result: []u8 = gpa.alloc(u8, 9) catch unreachable;
    for (inp.stacks) |stack, i| {
        if (stack.items.len > 0) {
            result[i] = stack.items[stack.items.len - 1];
        } else {
            result[i] = '_';
        }
    }
    return result[0..];
}

pub fn main() !void {
    var pinput = parse(input);
    std.debug.print("part 1: {s}\n", .{crateMover9000(pinput)});

    var pinput2 = parse(input);
    std.debug.print("part 2: {s}\n", .{crateMover9001(pinput2)});
}

// zig test a.zig
test "part 1" {
    var result = crateMover9000(parse(
        \\    [D]    
        \\[N] [C]    
        \\[Z] [M] [P]
        \\ 1   2   3 
        \\
        \\move 1 from 2 to 1
        \\move 3 from 1 to 3
        \\move 2 from 2 to 1
        \\move 1 from 1 to 2
    ));
    std.debug.print("\n{s}\n", .{result});
    try testing.expect(std.mem.eql(u8, result, "CMZ______"));
}

test "part 2" {
    var result = crateMover9001(parse(
        \\    [D]    
        \\[N] [C]    
        \\[Z] [M] [P]
        \\ 1   2   3 
        \\
        \\move 1 from 2 to 1
        \\move 3 from 1 to 3
        \\move 2 from 2 to 1
        \\move 1 from 1 to 2
    ));
    std.debug.print("\n{s}\n", .{result});
    try testing.expect(std.mem.eql(u8, result, "MCD______"));
}
