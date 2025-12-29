//! src/Token.zig

const Token = @This();

tag: Tag,
loc: Loc,

pub const Loc = struct {
    start: usize,
    end: usize,
};

pub const Tag = enum {
    invalid,
    l_paren,
    r_paren,
    comma,
    minus,
    plus,
    slash,
    star,
    equal,
    less,
    less_equal,
    less_greater,
    greater,
    greater_equal,
    greater_less,
    string_literal,
    number_literal,
    variable,
    keyword_print,
    keyword_if,
    keyword_then,
    keyword_goto,
    keyword_input,
    keyword_let,
    keyword_gosub,
    keyword_return,
    keyword_clear,
    keyword_list,
    keyword_run,
    keyword_end,
    eof,
};

pub const keywords: std.StaticStringMap(Tag) = .initComptime(&.{
    .{ "PRINT", .keyword_print },
    .{ "IF", .keyword_if },
    .{ "THEN", .keyword_then },
    .{ "GOTO", .keyword_goto },
    .{ "INPUT", .keyword_input },
    .{ "LET", .keyword_let },
    .{ "GOSUB", .keyword_gosub },
    .{ "RETURN", .keyword_return },
    .{ "CLEAR", .keyword_clear },
    .{ "LIST", .keyword_list },
    .{ "RUN", .keyword_run },
    .{ "END", .keyword_end },
});

pub fn getKeyword(bytes: []const u8) ?Tag {
    return keywords.get(bytes);
}

/// Scanner
pub const Scanner = struct {
    /// External reference to the tiny basic source code.
    src: [:0]const u8,

    index: usize = 0,

    const State = enum {
        start,
        string_literal,
        number_literal,
        identifier,
        less,
        greater,
        invalid,
    };

    fn isIndexAtEnd(s: Scanner) bool {
        if (s.index >= s.src.len) return true;
        return false;
    }

    fn peek(s: Scanner) u8 {
        return s.src[s.index];
    }

    fn advance(s: *Scanner) void {
        s.index += 1;
    }

    fn advanceThenPeek(s: *Scanner) u8 {
        s.advance();
        return s.peek();
    }

    pub fn next(s: *Scanner) Token {
        var result: Token = .{
            .loc = .{
                .start = s.index,
                .end = undefined,
            },
            .tag = undefined,
        };

        state: switch (State.start) {
            .start => switch (s.peek()) {
                0 => if (s.isIndexAtEnd()) {
                    return .{
                        .tag = .eof,
                        .loc = .{
                            .start = s.index,
                            .end = s.index,
                        },
                    };
                } else {
                    continue :state .invalid;
                },
                '\t', '\r', '\n', ' ' => {
                    s.advance();
                    result.loc.start = s.index;
                    continue :state .start;
                },
                '(' => {
                    s.advance();
                    result.tag = .l_paren;
                },
                ')' => {
                    s.advance();
                    result.tag = .r_paren;
                },
                ',' => {
                    s.advance();
                    result.tag = .comma;
                },
                '-' => {
                    s.advance();
                    result.tag = .minus;
                },
                '+' => {
                    s.advance();
                    result.tag = .plus;
                },
                '*' => {
                    s.advance();
                    result.tag = .star;
                },
                '/' => {
                    s.advance();
                    result.tag = .slash;
                },
                '=' => {
                    s.advance();
                    result.tag = .equal;
                },
                '>' => continue :state .greater,
                '<' => continue :state .less,
                '"' => {
                    result.tag = .string_literal;
                    continue :state .string_literal;
                },
                '0'...'9' => {
                    result.tag = .number_literal;
                    continue :state .number_literal;
                },
                'A'...'Z' => continue :state .identifier,
                else => continue :state .invalid,
            },
            .string_literal => {
                switch (s.advanceThenPeek()) {
                    0 => if (!s.isIndexAtEnd()) {
                        continue :state .invalid;
                    } else {
                        result.tag = .invalid;
                    },
                    '\n' => result.tag = .invalid,

                    '"' => s.advance(),
                    else => continue :state .string_literal,
                }
            },
            .number_literal => {
                switch (s.advanceThenPeek()) {
                    '0'...'9' => continue :state .number_literal,
                    else => {},
                }
            },
            .identifier => {
                switch (s.advanceThenPeek()) {
                    'A'...'Z' => continue :state .identifier,
                    else => {
                        const ident = s.src[result.loc.start..s.index];
                        if (ident.len == 1) {
                            result.tag = .variable;
                        } else if (Token.getKeyword(ident)) |tag| {
                            result.tag = tag;
                        } else if (s.isIndexAtEnd()) {
                            result.tag = .invalid;
                        } else {
                            continue :state .invalid;
                        }
                    },
                }
            },
            .less => {
                switch (s.advanceThenPeek()) {
                    '=' => {
                        s.advance();
                        result.tag = .less_equal;
                    },
                    '>' => {
                        s.advance();
                        result.tag = .less_greater;
                    },
                    else => result.tag = .less,
                }
            },
            .greater => {
                switch (s.advanceThenPeek()) {
                    '=' => {
                        s.advance();
                        result.tag = .greater_equal;
                    },
                    '<' => {
                        s.advance();
                        result.tag = .greater_less;
                    },
                    else => result.tag = .greater,
                }
            },
            .invalid => {
                switch (s.advanceThenPeek()) {
                    0 => if (s.isIndexAtEnd()) {
                        result.tag = .invalid;
                    } else {
                        continue :state .invalid;
                    },
                    '\n' => result.tag = .invalid,
                    else => continue :state .invalid,
                }
            },
        }

        result.loc.end = s.index;
        return result;
    }
};

const std = @import("std");
