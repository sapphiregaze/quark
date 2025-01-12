const std = @import("std");

const TokenType = enum {
    Key,
    Value,
    Colon,
    Dash,
    Indent,
    Dedent,
    Newline,
    Comment,
    EOF,
};

pub const Token = struct {
    type: TokenType,
    value: []const u8,
};

pub fn lexer(input: []const u8) ![]Token {
    const allocator = std.heap.page_allocator;

    var tokens = std.ArrayList(Token).init(allocator);
    defer tokens.deinit();

    try tokens.append(Token{ .type = .EOF, .value = "" });

    return tokens.toOwnedSlice();
}
