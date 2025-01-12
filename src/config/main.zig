const std = @import("std");

const lexer = @import("lexer.zig").lexer;
const parser = @import("parser.zig").parser;

pub fn load() !void {
    const allocator = std.heap.page_allocator;

    const cwd = std.fs.cwd();
    const buffer = try allocator.alloc(u8, 1024);
    const file_slice = try cwd.readFile("config.yaml", buffer);

    const tokens = try lexer(file_slice);
    const nodes = try parser(tokens);
}
