// run with:
// zig build-exe a.zig && ./a

const std = @import("std");

// embed the input data into the binary
// const data = @embedFile("sample.txt");
const data = @embedFile("input.txt");

// specialize a list on the type we're going to put in it
const EntriesList = std.ArrayList(i16);

pub fn main() !void {
    // create an allocator to store the result of parsing the input file
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    // parse the input into lines
    var lines = std.mem.tokenize(u8, data, "\r\n");

    // initialize the list
    var entries = EntriesList.init(gpa);

    // parse the file into lines, and the lines into numbers. Store into an
    // arraylist
    while (lines.next()) |line| {
        if (line.len == 0) continue;
        const num = try std.fmt.parseInt(i16, line, 10);
        try entries.append(num);
    }

    var items = entries.items;

    // count the increasing lines
    var n: usize = 0;
    for (items) |item, i| {
        if (i + 1 < items.len and item < items[i + 1]) {
            n += 1;
        }
    }

    std.debug.print("part 1: {}\n", .{n});

    // count the increasing triples
    var nn: usize = 0;
    for (items) |item, i| {
        if (i + 3 >= items.len) break;
        if (item + items[i + 1] + items[i + 2] <
            items[i + 1] + items[i + 2] + items[i + 3])
        {
            nn += 1;
        }
    }

    std.debug.print("part 2: {}\n", .{nn});
}
