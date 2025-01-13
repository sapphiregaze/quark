const std = @import("std");
const slice = @import("slice.zig");
const Allocator = std.mem.Allocator;

/// Represents a single token in the YAML stream.
/// Each token has a type indicating what kind of YAML element it represents,
/// and a value containing the actual text content (if any).
///
/// Token values point to slices of the original input string and do not own their memory.
/// The caller must ensure the input string outlives any usage of the token values.
pub const Token = struct {
    type: Type,
    value: []const u8,

    pub const Type = enum {
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
};

/// Lexer is responsible for tokenizing an input string according to YAML-like syntax.
/// It handles indentation, key-value pairs, list items, comments, and maintains proper
/// state during the lexing process.
pub const Lexer = struct {
    allocator: Allocator,
    input: []const u8,
    tokens: std.ArrayList(Token),
    pos: usize,
    curr_indent: usize,
    indent_stack: std.ArrayList(usize),

    /// Initializes a new Lexer instance with the given allocator and input string.
    /// Creates initial state for tracking indentation and tokens.
    ///
    /// Parameters:
    ///     allocator: Memory allocator for dynamic allocations
    ///     input: Source string to be lexed
    ///
    /// Returns: A new Lexer instance
    /// Error: Returns error if memory allocation fails
    pub fn init(allocator: Allocator, input: []const u8) !Lexer {
        var indent_stack = std.ArrayList(usize).init(allocator);
        try indent_stack.append(0);

        return Lexer{
            .allocator = allocator,
            .input = input,
            .tokens = std.ArrayList(Token).init(allocator),
            .pos = 0,
            .curr_indent = 0,
            .indent_stack = indent_stack,
        };
    }

    /// Cleans up all allocated resources used by the Lexer.
    /// This must be called when the Lexer is no longer needed to prevent memory leaks.
    pub fn deinit(self: *Lexer) void {
        self.tokens.deinit();
        self.indent_stack.deinit();
    }

    /// Main lexing function that processes the input string and produces a list of tokens.
    /// Handles all aspects of lexing including indentation, key-value pairs, and list items.
    ///
    /// Returns: Slice of Token objects representing the lexed input
    /// Error: Returns error if invalid syntax or indentation is encountered
    pub fn lex(self: *Lexer) ![]Token {
        while (self.pos < self.input.len) {
            if (self.skipEmptyLinesAndComments()) {
                continue;
            }

            try self.handleIndentation();
            if (self.isAtEnd() or self.isNewlineOrComment()) {
                self.skipUntilNewline();
                continue;
            }

            if (self.currentChar() == '-') {
                try self.handleListItem();
            } else {
                try self.handleKeyValuePair();
            }

            self.handleInlineComment();
            try self.handleNewline();
        }

        try self.finalizeDedents();
        try self.tokens.append(Token{ .type = .EOF, .value = "" });
        return self.tokens.toOwnedSlice();
    }

    /// Skips over empty lines and comment lines in the input.
    /// An empty line is one containing only whitespace or nothing.
    /// A comment line starts with '#' (possibly after whitespace).
    ///
    /// Returns: true if lines were skipped, false otherwise
    fn skipEmptyLinesAndComments(self: *Lexer) bool {
        var skipped = false;
        while (self.pos < self.input.len) {
            var spaces: usize = 0;
            const start_pos = self.pos;

            while (self.pos < self.input.len and self.input[self.pos] == ' ') {
                spaces += 1;
                self.pos += 1;
            }

            if (self.pos >= self.input.len or
                self.input[self.pos] == '\n' or
                self.input[self.pos] == '#')
            {
                while (self.pos < self.input.len and self.input[self.pos] != '\n') {
                    self.pos += 1;
                }
                if (self.pos < self.input.len) {
                    self.pos += 1;
                }
                skipped = true;
            } else {
                self.pos = start_pos;
                break;
            }
        }
        return skipped;
    }

    /// Handles indentation levels in the input, generating Indent and Dedent tokens as needed.
    /// Maintains a stack of indentation levels to properly track nested structures.
    ///
    /// Error: Returns error.InvalidIndentation if indentation is inconsistent
    fn handleIndentation(self: *Lexer) !void {
        var spaces: usize = 0;
        while (self.pos < self.input.len and self.input[self.pos] == ' ') {
            spaces += 1;
            self.pos += 1;
        }

        const new_indent = spaces;
        if (new_indent > self.curr_indent) {
            try self.tokens.append(Token{ .type = .Indent, .value = "" });
            try self.indent_stack.append(new_indent);
            self.curr_indent = new_indent;
        } else if (new_indent < self.curr_indent) {
            while (self.indent_stack.items.len > 0 and self.curr_indent > new_indent) {
                try self.tokens.append(Token{ .type = .Dedent, .value = "" });
                _ = self.indent_stack.pop();

                self.curr_indent = if (self.indent_stack.items.len > 0)
                    self.indent_stack.items[self.indent_stack.items.len - 1]
                else
                    0;
            }
            if (self.curr_indent != new_indent) {
                return error.InvalidIndentation;
            }
        }
    }

    /// Processes a list item starting with '-' and extracts its value.
    /// Generates Dash and Value tokens for the list item.
    ///
    /// Error: Returns error if memory allocation fails during token creation
    fn handleListItem(self: *Lexer) !void {
        try self.tokens.append(Token{ .type = .Dash, .value = "-" });
        self.pos += 1;

        while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;

        const value_start = self.pos;
        while (self.pos < self.input.len and !self.isNewlineOrComment()) self.pos += 1;
        const value = self.input[value_start..self.pos];

        if (value.len > 0) {
            try self.tokens.append(Token{ .type = .Value, .value = slice.trim(value) });
        }
    }

    /// Processes a key-value pair in the format "key: value".
    /// Generates Key, Colon, and Value tokens.
    ///
    /// Error: Returns error.InvalidSyntax if the key-value format is invalid
    fn handleKeyValuePair(self: *Lexer) !void {
        const key_start = self.pos;
        while (self.pos < self.input.len and !self.isColonOrNewlineOrSpace()) self.pos += 1;

        const key = self.input[key_start..self.pos];

        while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;

        if (self.pos < self.input.len and self.input[self.pos] == ':') {
            try self.tokens.append(Token{ .type = .Key, .value = key });
            try self.tokens.append(Token{ .type = .Colon, .value = ":" });
            self.pos += 1;

            while (self.pos < self.input.len and self.input[self.pos] == ' ') self.pos += 1;

            const value_start = self.pos;
            while (self.pos < self.input.len and !self.isNewlineOrComment()) self.pos += 1;
            const value = self.input[value_start..self.pos];

            if (value.len > 0) {
                try self.tokens.append(Token{ .type = .Value, .value = slice.trim(value) });
            }
        } else {
            return error.InvalidSyntax;
        }
    }

    /// Skips over inline comments (starting with '#') until the end of the line
    fn handleInlineComment(self: *Lexer) void {
        if (self.pos < self.input.len and self.input[self.pos] == '#') {
            while (self.pos < self.input.len and self.input[self.pos] != '\n') self.pos += 1;
        }
    }

    /// Processes newline characters and generates Newline tokens
    ///
    /// Error: Returns error if memory allocation fails during token creation
    fn handleNewline(self: *Lexer) !void {
        if (self.pos < self.input.len and self.input[self.pos] == '\n') {
            try self.tokens.append(Token{ .type = .Newline, .value = "\n" });
            self.pos += 1;
        }
    }

    /// Generates any remaining Dedent tokens needed to close nested structures
    /// when reaching the end of input
    ///
    /// Error: Returns error if memory allocation fails during token creation
    fn finalizeDedents(self: *Lexer) !void {
        while (self.indent_stack.items.len > 1) {
            try self.tokens.append(Token{ .type = .Dedent, .value = "" });
            _ = self.indent_stack.pop();
        }
    }

    // Skips until the end has been reached or it has arrived at a newline character
    fn skipUntilNewline(self: *Lexer) void {
        while (self.pos < self.input.len and self.input[self.pos] != '\n') self.pos += 1;
        if (self.pos < self.input.len) self.pos += 1;
    }

    /// Checks if the lexer has reached the end of the input
    fn isAtEnd(self: *const Lexer) bool {
        return self.pos >= self.input.len;
    }

    /// Returns the current character being processed, or 0 if at end of input
    fn currentChar(self: *const Lexer) u8 {
        return if (self.pos < self.input.len) self.input[self.pos] else 0;
    }

    /// Checks if the current character is a newline or comment marker
    fn isNewlineOrComment(self: *const Lexer) bool {
        return self.currentChar() == '\n' or self.currentChar() == '#';
    }

    /// Checks if the current character is a colon, newline, or space
    fn isColonOrNewlineOrSpace(self: *const Lexer) bool {
        const c = self.currentChar();
        return c == ':' or c == '\n' or c == ' ';
    }
};

/// Compares tokens from 2 slices and test if they are equal
fn compareTokens(expected: []const Token, actual: []const Token) !void {
    const testing = std.testing;

    try testing.expectEqual(expected.len, actual.len);
    for (expected, 0..) |exp_token, i| {
        try testing.expectEqual(exp_token.type, actual[i].type);
        try testing.expectEqualSlices(u8, exp_token.value, actual[i].value);
    }
}

test "empty input" {
    const testing = std.testing;

    const input = "";

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "only whitespace and newlines" {
    const testing = std.testing;

    const input =
        \\  
        \\    
        \\
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "only comments" {
    const testing = std.testing;

    const input =
        \\# comment 1
        \\  # comment 2
        \\    # comment 3
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "simple key-value" {
    const testing = std.testing;

    const input = "key: value";

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .Key, .value = "key" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "value" },
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "key with no value" {
    const testing = std.testing;

    const input = "key:";

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .Key, .value = "key" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "nested structure" {
    const testing = std.testing;

    const input =
        \\outer:
        \\  inner1: value1
        \\  inner2:
        \\    deepnested: value2
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .Key, .value = "outer" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Key, .value = "inner1" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "value1" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Key, .value = "inner2" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Key, .value = "deepnested" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "value2" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "list items" {
    const testing = std.testing;

    const input =
        \\items:
        \\  - item1
        \\  - item2
        \\  - 
        \\  -    spaced item
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .Key, .value = "items" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "item1" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "item2" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "spaced item" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "mixed content" {
    const testing = std.testing;

    const input =
        \\ # Regular comment
        \\name: value # With inline comment
        \\address:  
        \\  street: some street
        \\  city: some city
        \\favorites:
        \\  colors:
        \\    - purple # More inline comment
        \\    - green
        \\  numbers: # Last one
        \\    - 42
        \\    - 
        \\    - 7
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();
    const tokens = try testLexer.lex();
    defer testing.allocator.free(tokens);

    const expected = &[_]Token{
        .{ .type = .Key, .value = "name" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "value" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Key, .value = "address" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Key, .value = "street" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "some street" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Key, .value = "city" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Value, .value = "some city" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .Key, .value = "favorites" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Key, .value = "colors" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "purple" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "green" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .Key, .value = "numbers" },
        .{ .type = .Colon, .value = ":" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Indent, .value = "" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "42" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Newline, .value = "\n" },
        .{ .type = .Dash, .value = "-" },
        .{ .type = .Value, .value = "7" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .Dedent, .value = "" },
        .{ .type = .EOF, .value = "" },
    };
    try compareTokens(expected, tokens);
}

test "invalid indentation" {
    const testing = std.testing;

    const input =
        \\key1:
        \\      - value1
        \\  - value2
    ;

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();

    try testing.expectError(error.InvalidIndentation, testLexer.lex());
}

test "invalid syntax - missing colon" {
    const testing = std.testing;

    const input = "key value";

    var testLexer = try Lexer.init(testing.allocator, input);
    defer testLexer.deinit();

    try testing.expectError(error.InvalidSyntax, testLexer.lex());
}
