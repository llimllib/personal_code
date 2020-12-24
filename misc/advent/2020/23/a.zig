const std = @import("std");
const stdout = std.io.getStdOut().writer();
const alloc = &(std.heap.GeneralPurposeAllocator(.{}){}).allocator;
const print = @import("std").debug.print;

pub fn p(rnd: usize, ns: []usize, cur: usize) void {
    stdout.print("{}: ", .{rnd}) catch unreachable;
    for (ns) |n, i| {
        if (i == cur) {
            stdout.print("({}) ", .{n}) catch unreachable;
        } else {
            stdout.print("{} ", .{n}) catch unreachable;
        }
    }
    stdout.print("\n", .{}) catch unreachable;
}

pub fn loop(ns: []usize, rounds: usize) []usize {
    var cur: usize = 0;
    // we assume that the length of the slice is equal to the max element of the slice
    var l: usize = ns.len;
    var n3 = [3]usize{ 0, 0, 0 };

    var i: usize = 0;
    while (i < rounds) : (i += 1) {
        if ((i + 1) % 100000 == 0) {
            print("{}\n", .{i + 1});
        }
        // p(i + 1, ns, cur);
        n3[0] = ns[(cur + 1) % l];
        n3[1] = ns[(cur + 2) % l];
        n3[2] = ns[(cur + 3) % l];
        ns[(cur + 1) % l] = 0;
        ns[(cur + 2) % l] = 0;
        ns[(cur + 3) % l] = 0;

        // print("n3: {} {} {}\n", .{ n3[0], n3[1], n3[2] });

        // select the destination cup
        var destcup = ns[cur] - 1;
        if (destcup == 0) {
            destcup = l;
        }
        while (n3[0] == destcup or n3[1] == destcup or n3[2] == destcup) {
            // print("destcup: {}\n", .{destcup});
            destcup -= 1;
            if (destcup == 0) {
                destcup = l;
            }
        }

        // find the destination cup in the slice
        var destidx: usize = 0;
        for (ns) |n, j| {
            if (n == destcup) {
                destidx = j;
                break;
            }
        }

        // print("destidx: {} {}\n", .{ destidx, ns[destidx] });

        if (cur > destidx) {
            var j: usize = cur;
            while (j > destidx) : (j -= 1) {
                ns[(j + 3) % l] = ns[j];
            }
        } else {
            var j: usize = cur + l;
            while (j > destidx) : (j -= 1) {
                if (j >= l) {
                    ns[(j % l) + 3] = ns[j % l];
                } else {
                    ns[(j + 3) % l] = ns[j];
                }
            }
        }

        cur = (cur + 3) % l;

        ns[(destidx + 1) % l] = n3[0];
        ns[(destidx + 2) % l] = n3[1];
        ns[(destidx + 3) % l] = n3[2];
        cur = (cur + 1) % l;
    }

    return ns;
}

test "loop" {
    var inp = [_]usize{ 3, 8, 9, 1, 2, 5, 4, 6, 7 };
    var s = loop(inp[0..], 10);
    var expected = [_]usize{ 9, 2, 6, 5, 8, 3, 7, 4, 1 };
    print("result: ", .{});
    for (s) |n| {
        print("{} ", .{n});
    }
    print("\n", .{});
    std.testing.expect(std.mem.eql(usize, s, expected[0..]));
}

pub fn main() !void {
    // 653427918
    var inp = [_]usize{ 6, 5, 3, 4, 2, 7, 9, 1, 8 };
    var s = loop(inp[0..], 100);
    print("part 1: ", .{});
    for (s) |n| {
        print("{} ", .{n});
    }
    print("\n", .{});

    var biginp: [1_000_000]usize = undefined;
    var i: usize = 0;
    while (i < 9) : (i += 1) {
        biginp[i] = inp[i];
    }
    std.testing.expect(i == 9);
    while (i < 1_000_000) : (i += 1) {
        biginp[i] = i;
    }
    s = loop(biginp[0..], 10_000_000);
    print("part 2: ", .{});
    for (s) |j, n| {
        if (n == 1) {
            var a = s[(j + 1) % 1_000_000];
            var b = s[(j + 2) % 1_000_000];
            print("{} {} {}", .{ a, b, a * b });
            break;
        }
    }
    print("\n", .{});
}
