const std = @import("std");
const Lexer = @import("lexer.zig").Lexer;
const parser = @import("parser.zig").parser;

pub fn load() !void {
    const allocator = std.heap.page_allocator;

    const buffer = try allocator.alloc(u8, 1024);
    defer allocator.free(buffer);

    const cwd = std.fs.cwd();
    const file_slice = try cwd.readFile("config.yaml", buffer);

    var lexer = try Lexer.init(allocator, file_slice);
    defer lexer.deinit();
    const tokens = try lexer.lex();

    std.debug.print("{any}", .{tokens});

    const nodes = try parser(tokens);
    std.debug.print("{any}", .{nodes});
}
