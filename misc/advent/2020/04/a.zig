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
const PassportMap = std.StringHashMap([]const u8);
const PassportList = std.ArrayList(PassportMap);

// given an input buffer, return the number of valid passport entries
fn valid1(passports: PassportList) usize {
    const required = &[_][]const u8{ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" };
    var valid: usize = 0;
    outer: for (passports.items) |passport| {
        for (required) |key| {
            if (passport.get(key) == null) {
                continue :outer;
            }
        }
        valid += 1;
    }
    return valid;
}

fn find(haystack: *const [7][]const u8, needle: []const u8) bool {
    for (haystack) |elt| {
        if (std.mem.eql(u8, elt, needle)) {
            return true;
        }
    }
    return false;
}

fn valid2(passports: PassportList) !usize {
    const required = &[_][]const u8{ "byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid" };
    const validEcl = &[_][]const u8{ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" };
    var valid: usize = 0;
    invalid: for (passports.items) |passport| {
        for (required) |key| {
            if (passport.get(key) == null) {
                continue :invalid;
            }
        }
        var byr = try std.fmt.parseInt(usize, passport.get("byr").?, 10);
        if (byr < 1920 or byr > 2002) {
            continue :invalid;
        }
        var iyr = try std.fmt.parseInt(usize, passport.get("iyr").?, 10);
        if (iyr < 2010 or iyr > 2020) {
            continue :invalid;
        }
        var eyr = try std.fmt.parseInt(usize, passport.get("eyr").?, 10);
        if (eyr < 2020 or eyr > 2030) {
            continue :invalid;
        }

        var parts = std.mem.tokenize(passport.get("hgt").?, "ci");
        var h = try std.fmt.parseInt(usize, parts.next().?, 10);
        var unit = parts.next();
        if (unit == null) {
            continue :invalid;
        }
        if ((std.mem.eql(u8, unit.?, "in") and (h < 59 or h > 76)) or (std.mem.eql(u8, unit.?, "cm") and (h < 150 or h > 193))) {
            continue :invalid;
        }

        var hcl = passport.get("hcl").?;
        if (hcl.len != 7 or hcl[0] != '#') {
            continue :invalid;
        }
        var i: usize = 6;
        while (i > 0) : (i -= 1) {
            if (!std.ascii.isDigit(hcl[i]) and (hcl[i] < 'a' or hcl[i] > 'f')) {
                std.debug.print("{} {}\n", .{ hcl[i], hcl });
                continue :invalid;
            }
        }

        if (!find(validEcl, passport.get("ecl").?)) {
            continue :invalid;
        }

        valid += 1;
    }
    return valid;
}

fn parse(allocator: *std.mem.Allocator, input: []u8) !std.ArrayList(PassportMap) {
    var passports = PassportList.init(allocator);
    var passport_entries = std.mem.split(input, "\n\n");
    var passport = PassportMap.init(allocator);
    while (passport_entries.next()) |entry| {
        var data = std.mem.tokenize(entry, " :\n");
        while (data.next()) |key| {
            var val = data.next().?;
            try passport.put(key, val);
        }
        try passports.append(passport);
        passport = PassportMap.init(allocator);
    }

    return passports;
}

pub fn main() !void {
    var alloc = std.heap.GeneralPurposeAllocator(.{}){};
    var args = try std.process.argsAlloc(&alloc.allocator);
    const input = try std.fs.cwd().readFileAlloc(&alloc.allocator, args[1], std.math.maxInt(usize));

    var passports = try parse(&alloc.allocator, input);
    std.debug.print("total: {}\n", .{passports.items.len});
    std.debug.print("valid part 1: {}\n", .{valid1(passports)});
    std.debug.print("valid part 2: {}\n", .{valid2(passports)});
    // XXX: function to print hash entry nicely
    // var it = passports.items[0].iterator();
    // while (it.next()) |key| {
    //     std.debug.print("{}\n", .{key});
    // }
}
