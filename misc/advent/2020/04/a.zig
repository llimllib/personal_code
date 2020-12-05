const std = @import("std");
const fs = std.fs;

// byr (Birth Year)
// iyr (Issue Year)
// eyr (Expiration Year)
// hgt (Height)
// hcl (Hair Color)
// ecl (Eye Color)
// pid (Passport ID)
// cid (Country ID)
const Passport = struct {
    byr: usize,
    iyr: usize,
    eyr: usize,
    hgt: usize,
    hgtUnit: []u8,
    hcl: []u8,
    ecl: []u8,
    pid: []u8,
    cid: []u8,
};

// given an input buffer, return the number of valid passport entries
fn valid1(input: []Passport) usize {}

fn parse(allocator: *std.mem.Allocator, input: []u8) !std.ArrayList(Passport) {
    var passports = std.ArrayList(Passport).init(allocator);
    var lines = std.mem.split(input, "\n");
    var passport = Passport{
        .byr = 0,
        .iyr = 0,
        .eyr = 0,
        .hgt = 0,
        .hgtUnit = "",
        .hcl = "",
        .ecl = "",
        .pid = "",
        .cid = "",
    };
    while (lines.next()) |line| {
        if (line.len == 0) {
            try passports.append(passport);
            passport = Passport{
                .byr = 0,
                .iyr = 0,
                .eyr = 0,
                .hgt = 0,
                .hgtUnit = "",
                .hcl = "",
                .ecl = "",
                .pid = "",
                .cid = "",
            };
            continue;
        }

        var data = std.mem.tokenize(line, " :");
        while (data.next()) |key| {
            var val = data.next().?;
            if (std.mem.eql(u8, key, "byr")) {
                passport.byr = try std.fmt.parseInt(usize, val, 10);
            }
        }
    }

    return passports;
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var args = try std.process.argsAlloc(&alloc.allocator);
    const input = try std.fs.cwd().readFileAlloc(&alloc.allocator, args[1], std.math.maxInt(usize));

    var passports = try parse(&alloc.allocator, input);
    std.debug.print("{}\n", .{passports.items.len});
}
