const std = @import("std");
const config = @import("config/main.zig");

pub fn main() !void {
    try config.load();
}
