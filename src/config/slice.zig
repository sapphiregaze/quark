const std = @import("std");

/// Trims empty spaces from the beginning and the end of a slice
pub fn trim(slice: []const u8) []const u8 {
    var start: usize = 0;
    var end: usize = slice.len;

    while (start < end and slice[start] == ' ') {
        start += 1;
    }

    while (end > start and slice[end - 1] == ' ') {
        end -= 1;
    }

    return slice[start..end];
}

test "trim" {
    const testing = std.testing;

    const input = "  hello world  ";
    const result = trim(input);
    try testing.expectEqualSlices(u8, "hello world", result);
}
