const std = @import("std");
const fs = std.fs;

const RegexError = error{InvalidEscape};

const State = struct {
    accept: bool,
    char: ?u8,
    // https://swtch.com/~rsc/regexp/regexp1.html
    // can we reduce this to just two out pointers? do we ever want our states
    // to have more than two ouptuts / can they?
    transitions: StateList,
    mark: bool,
};

const StateList = std.ArrayList(*State);

const Match = std.ArrayList([]const u8);

const Regex = struct {
    regex: []const u8,
    nfa: *State,
    alloc: *std.mem.Allocator,

    // TODO: properly free memory. create docs say:
    //      /// Returns a pointer to undefined memory.
    //      /// Call `destroy` with the result to free the memory.
    pub fn compile(comptime regex: []const u8, alloc: *std.mem.Allocator) !Regex {
        var cur: usize = 0;
        const root = try alloc.create(State);
        root.* = .{
            .accept = false,
            .char = null,
            .transitions = StateList.init(alloc),
            .mark = false,
        };
        var curState: *State = root;

        while (cur < regex.len) : (cur += 1) {
            if (regex[cur] == '.') {
                const st = try alloc.create(State);
                st.* = .{
                    .accept = false,
                    .transitions = StateList.init(alloc),
                    .char = '.',
                    .mark = false,
                };
                try curState.transitions.append(st);
                curState = st;
            } else if (regex[cur] == '*') {
                try curState.transitions.append(curState);
            } else {
                const st = try alloc.create(State);
                st.* = .{
                    .accept = false,
                    .transitions = StateList.init(alloc),
                    .char = regex[cur],
                    .mark = false,
                };
                try curState.transitions.append(st);
                curState = st;
            }
        }
        curState.accept = true;
        return Regex{ .regex = regex, .nfa = root, .alloc = alloc };
    }

    // XXX: does this accurately destroy all states?
    fn destroyState(self: *Regex, state: *State) void {
        // mark the node so we don't recurse infinitely
        state.mark = true;
        for (state.transitions.items) |rule| {
            if (!state.mark) {
                self.destroyState(rule);
            }
        }
        state.transitions.deinit();
        self.alloc.destroy(state);
    }

    pub fn destroy(self: *Regex) void {
        self.destroyState(self.nfa);
    }

    // match returns null if s does not match the regex, and a Match object
    // otherwise
    pub fn match(self: *Regex, s: []const u8) !?Match {
        var groups: Match = Match.init(self.alloc);
        var cur: usize = 0;
        var state = self.nfa;
        for (s) |c| {
            for (state.transitions.items) |rule| {
                var toMatch = rule.char.?;
                if (toMatch == '.') {
                    std.debug.print("rule.char . {u}\n", .{c});
                    state = rule;
                } else if (toMatch == c) {
                    std.debug.print("rule.eq {u} {u}\n", .{ toMatch, c });
                    state = rule;
                } else {
                    std.debug.print("no match {u} {u}\n", .{ toMatch, c });
                    return null;
                }
            }
        }
        if (!state.accept) {
            std.debug.print("not an accept state\n", .{});
            return null;
        }
        return groups;
    }
};

test "simple examples" {
    // var re = try Regex.compile("bananas", std.testing.allocator);
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var re = try Regex.compile("bananas", &alloc.allocator);
    defer re.destroy();
    std.testing.expect((try re.match("bananas")) != null);
    std.testing.expect((try re.match("banter")) == null);
    std.testing.expect((try re.match("ban")) == null);
}

test "dot character" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var re = try Regex.compile("ba..nas", &alloc.allocator);
    defer re.destroy();
    std.testing.expect((try re.match("bananas")) != null);
    std.testing.expect((try re.match("banter")) == null);
    std.testing.expect((try re.match("ban")) == null);
}

test "repetition" {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var re = try Regex.compile("ab*d", &alloc.allocator);
    defer re.destroy();
    std.testing.expect((try re.match("ad")) != null);
    std.testing.expect((try re.match("abd")) != null);
    std.testing.expect((try re.match("abbbbbd")) != null);
    std.testing.expect((try re.match("acd")) == null);
    std.testing.expect((try re.match("cd")) == null);
    std.testing.expect((try re.match("ab")) == null);
}

// test "memory fuuuuuu" {
//     // XXX: destroy looks to me like it should destroy all State objects, but
//     // still fails with a memory leak. Not sure how to figure this out
//     var re = try Regex.compile("b", std.testing.allocator);
//     defer re.destroy();
//     std.testing.expect((try re.match("b")) != null);
// }

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
