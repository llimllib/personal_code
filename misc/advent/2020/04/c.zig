const std = @import("std");
const fs = std.fs;

const PassportMap = std.StringHashMap([]const u8);
const PassportList = std.ArrayList(PassportMap);
const RegexError = error{InvalidEscape};

const State = struct {
    accept: bool,
    char: ?u8,
    transitions: StateList,
};

const StateList = std.ArrayList(*State);

const DFA = std.StringHashMap(*State);

const Match = std.ArrayList([]const u8);

const Regex = struct {
    regex: []const u8,
    dfa: *State,
    alloc: *std.mem.Allocator,

    // TODO: properly free memory (probably accept allocator and give a `free` method?)
    pub fn compile(comptime regex: []const u8, alloc: *std.mem.Allocator) !Regex {
        var cur: usize = 0;
        const root = try alloc.create(State);
        root.* = .{ .accept = false, .char = null, .transitions = StateList.init(alloc) };
        var curState: *State = root;

        while (cur < regex.len) : (cur += 1) {
            if (regex[cur] == '.') {
                const st = try alloc.create(State);
                st.* = .{
                    .accept = false,
                    .transitions = StateList.init(alloc),
                    .char = '.',
                };
                try curState.transitions.append(st);
                curState = st;
            } else {
                const st = try alloc.create(State);
                st.* = .{
                    .accept = false,
                    .transitions = StateList.init(alloc),
                    .char = regex[cur],
                };
                try curState.transitions.append(st);
                curState = st;
            }
        }
        curState.accept = true;
        return Regex{ .regex = regex, .dfa = root, .alloc = alloc };
    }

    pub fn match(self: *Regex, s: []const u8) !?Match {
        var groups: Match = Match.init(self.alloc);
        var cur: usize = 0;
        for (self.dfa.transitions.items) |rule| {
            if (rule.char.? == '.') {
                cur += 1;
            } else if (rule.char.? == s[cur]) {
                cur += 1;
            } else {
                std.debug.print("returning null\n", .{});
                return null;
            }
        }
        return groups;
    }
};

test "compile" {
    var re = try Regex.compile("bananas", std.testing.allocator);
    std.testing.expect((try re.match("bananas")) != null);
    std.testing.expect((try re.match("stardust")) == null);
}

fn match(regex: []const u8, s: []const u8) RegexError!bool {
    var rcur: usize = 0;
    var scur: usize = 0;
    while (rcur < regex.len) : (rcur += 1) {
        if (scur >= s.len) {
            return false;
        }
        if (regex[rcur] == '.') {
            scur += 1;
        } else if (regex[rcur] == '\\') {
            rcur += 1;
            if (regex[rcur] == 'd') {
                if (std.ascii.isDigit(s[scur])) {
                    scur += 1;
                }
            } else {
                return RegexError.InvalidEscape;
            }
        } else if (regex[rcur] == s[scur]) {
            scur += 1;
        } else {
            return false;
        }
    }
    return true;
}

test "match" {
    std.testing.expect(try match("bananas", "bananas"));
    std.testing.expect(try match("ban", "bananas"));
    std.testing.expect(!try match("bananas", "ban"));
    std.testing.expect(!try match("bananas", "cream"));

    std.testing.expect(try match("..nanas", "bananas"));
    std.testing.expect(!try match("..anas", "bananas"));

    std.testing.expect(try match("all\\done", "all4one"));
    std.testing.expect(!try match("all\\done", "allfone"));
    std.testing.expectError(RegexError.InvalidEscape, match("test\\zing", "testcapade"));
}
