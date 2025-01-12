const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const Node = struct {
    key: ?[]const u8,
    value: ?[]const u8,
    children: ?[]Node,
};

pub fn parser(tokens: []Token) ![]Node {}
