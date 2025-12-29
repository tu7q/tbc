//! src/Parse.zig

gpa: Allocator,
source: []const u8,
tokens: Ast.TokenList.Slice,
tok_i: TokenIndex,
errors: std.ArrayListUnmanaged(AstError),
nodes: Ast.NodeList,
extra_data: std.ArrayListUnmanaged(u32),
scratch: std.ArrayListUnmanaged(u32),

pub const Error = error{ParseError} || Allocator.Error;

fn tokenTag(p: *const Parse, token_index: TokenIndex) Token.Tag {
    return p.tokens.items(.tag)[token_index];
}

fn tokenStart(p: *const Parse, token_index: TokenIndex) Ast.ByteOffset {
    return p.tokens.items(.start)[token_index];
}

fn nodeTag(p: *const Parse, node: Node.Index) Node.Tag {
    return p.nodes.items(.tag)[@intFromEnum(node)];
}

fn nodeMainToken(p: *const Parse, node: Node.Index) TokenIndex {
    return p.nodes.items(.main_token)[@intFromEnum(node)];
}

fn nodeData(p: *const Parse, node: Node.Index) Node.Data {
    return p.nodes.items(.data)[@intFromEnum(node)];
}

fn addNode(p: *Parse, elem: Ast.Node) Allocator.Error!Node.Index {
    const result: Node.Index = @enumFromInt(p.nodes.len);
    try p.nodes.append(p.gpa, elem);
    return result;
}

fn moveScratchToExtra(p: *Parse, items: []const u32) Allocator.Error!Node.SubRange {
    try p.extra_data.appendSlice(p.gpa, items);
    return .{
        .start = @enumFromInt(p.extra_data.items.len - items.len),
        .end = @enumFromInt(p.extra_data.items.len),
    };
}

fn tokensOnSameLine(p: *const Parse, a: TokenIndex, b: TokenIndex) bool {
    return std.mem.indexOfScalar(u8, p.source[p.tokenStart(a)..p.tokenStart(b)], '\n') == null;
}

fn preceededByTags(p: *const Parse, token_index: TokenIndex, tags: []const Token.Tag) bool {
    return std.mem.endsWith(Token.Tag, p.tokens.items(.tag)[0..token_index], tags);
}

fn warnExpected(p: *Parse, expected_token: Token.Tag) Error {
    @branchHint(.cold);
    return p.warnMsg(.{
        .tag = .expected_token,
        .token = p.tok_i,
        .extra = .{
            .expected_tag = expected_token,
        },
    });
}

fn warn(p: *Parse, error_tag: Ast.Error.Tag) Error {
    @branchHint(.cold);
    return p.warnMsg(.{
        .tag = error_tag,
        .token = p.tok_i,
        .extra = .{ .none = {} },
    });
}

fn warnMsg(p: *Parse, msg: Ast.Error) Error {
    @branchHint(.cold);
    try p.errors.append(p.gpa, msg);
    return error.ParseError;
}

fn eatToken(p: *Parse, tag: Token.Tag) ?TokenIndex {
    return if (p.tokenTag(p.tok_i) == tag) p.nextToken() else null;
}

fn nextToken(p: *Parse) TokenIndex {
    const result = p.tok_i;
    p.tok_i += 1;
    return result;
}

/// Root
pub fn parseRoot(p: *Parse) Allocator.Error!void {
    assert(p.nodes.len == 0);

    // Root node must be index 0.
    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });

    const statements = try p.parseTopLevelStmts();
    p.nodes.items(.data)[0] = statements;
}

/// Parses a single top level statement that the main ptr of
/// node will point to. Note that if there is any error the
/// statement will be undefined.
pub fn parseChunk(p: *Parse) Allocator.Error!void {
    assert(p.nodes.len == 0);

    p.nodes.appendAssumeCapacity(.{
        .tag = .root,
        .main_token = 0,
        .data = undefined,
    });

    if (p.parseTopLevelStmt()) |stmt| {
        p.nodes.items(.data)[0] = .{ .node = stmt };
    } else |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ParseError => {},
    }
}

fn parseTopLevelStmts(p: *Parse) Allocator.Error!Node.Data {
    assert(p.scratch.items.len == 0);
    defer p.scratch.clearRetainingCapacity();

    while (true) {
        switch (p.tokenTag(p.tok_i)) {
            .eof => break,
            else => {
                const stmt = p.parseTopLevelStmt() catch |err| switch (err) {
                    error.ParseError => {
                        p.findNextTopLevelStmt();
                        continue;
                    },
                    error.OutOfMemory => return error.OutOfMemory,
                };

                try p.scratch.append(p.gpa, @intFromEnum(stmt));
            },
        }
    }

    const range = try p.moveScratchToExtra(p.scratch.items);
    return .{ .extra_range = range };
}

fn parseTopLevelStmt(p: *Parse) Error!Node.Index {
    const line = p.eatToken(.number_literal);
    const stmt = try p.expectStmt();

    var top = stmt;

    if (line) |some| {
        var scanner: Scanner = .{
            .src = @ptrCast(p.source),
            .index = p.tokenStart(some),
        };
        const loc = scanner.next().loc;
        if (std.mem.allEqual(u8, p.source[loc.start..loc.end], '0')) {
            try p.errors.append(p.gpa, .{
                .tag = .zero_line_number,
                .token = some,
                .extra = .{ .none = {} },
            });
        }
        top = try p.addNode(.{
            .tag = .defered_statement,
            .main_token = some,
            .data = .{ .node = stmt },
        });
    }

    return top;
}

///
fn expectStmt(p: *Parse) Error!Node.Index {
    switch (p.tokenTag(p.tok_i)) {
        .keyword_print => {
            const print_token = p.nextToken();
            return p.addNode(.{
                .tag = .print,
                .main_token = print_token,
                .data = .{ .extra_range = try p.expectExprList() },
            });
        },
        .keyword_if => {
            const if_token = p.tok_i;
            p.tok_i += 1;
            const condition = try p.expectCondition();
            _ = p.eatToken(.keyword_then) orelse return p.warn(.expected_then);
            const then = try p.expectStmt();
            return p.addNode(.{
                .tag = .@"if",
                .main_token = if_token,
                .data = .{ .node_and_node = .{ condition, then } },
            });
        },
        .keyword_goto, .keyword_gosub => |token| {
            const goto_or_gosub_token = p.nextToken();
            return p.addNode(.{
                .tag = switch (token) {
                    .keyword_goto => .goto,
                    .keyword_gosub => .gosub,
                    else => unreachable,
                },
                .main_token = goto_or_gosub_token,
                .data = .{ .node = try p.expectExpr() },
            });
        },
        .keyword_input => {
            const input_token = p.nextToken();
            return p.addNode(.{
                .tag = .input,
                .main_token = input_token,
                .data = .{ .extra_range = try p.expectVarList() },
            });
        },
        .keyword_let => {
            const let_token = p.nextToken();
            const variable = p.eatToken(.variable) orelse return p.warn(.expected_variable);
            _ = p.eatToken(.equal) orelse return p.warn(.expected_equal);
            const expr = try p.expectExpr();
            return p.addNode(.{
                .tag = .let,
                .main_token = let_token,
                .data = .{ .node_and_token = .{ expr, variable } },
            });
        },
        .keyword_return,
        .keyword_clear,
        .keyword_list,
        .keyword_run,
        .keyword_end,
        => |token| {
            return p.addNode(.{
                .tag = switch (token) {
                    .keyword_return => .@"return",
                    .keyword_clear => .clear,
                    .keyword_list => .list,
                    .keyword_run => .run,
                    .keyword_end => .end,
                    else => unreachable,
                },
                .main_token = p.nextToken(),
                .data = undefined,
            });
        },

        // Error.
        else => return p.warn(.expected_stmt),
    }
}

/// Does its best to find the next top level statement.
fn findNextTopLevelStmt(p: *Parse) void {
    while (true) {
        const tok = p.nextToken();
        switch (p.tokenTag(tok)) {
            .keyword_print,
            .keyword_if,
            .keyword_goto,
            .keyword_input,
            .keyword_let,
            .keyword_gosub,
            .keyword_return,
            .keyword_clear,
            .keyword_list,
            .keyword_run,
            .keyword_end,
            => {
                if (!p.preceededByTags(tok, &.{.keyword_then})) {
                    p.tok_i -= 1;
                    return;
                }
            },
            .eof => {
                p.tok_i -= 1;
                return;
            },
            else => {},
        }
    }
}

fn expectExprList(p: *Parse) Error!Node.SubRange {
    const top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(top);

    const head = try p.expectExprListElem();
    try p.scratch.append(p.gpa, @intFromEnum(head));

    while (p.eatToken(.comma)) |_| {
        const next = try p.expectExprListElem();
        try p.scratch.append(p.gpa, @intFromEnum(next));
    }

    return try p.moveScratchToExtra(p.scratch.items[top..]);
}

fn expectExprListElem(p: *Parse) Error!Node.Index {
    switch (p.tokenTag(p.tok_i)) {
        .string_literal => {
            return p.addNode(.{
                .tag = .string_literal,
                .main_token = p.nextToken(),
                .data = undefined,
            });
        },
        else => return p.expectExpr(),
    }
}

fn expectVarList(p: *Parse) Error!Node.SubRange {
    const top = p.scratch.items.len;
    defer p.scratch.shrinkRetainingCapacity(top);

    const head: TokenIndex = p.eatToken(.variable) orelse return p.warn(.expected_variable);
    try p.scratch.append(p.gpa, head);

    while (p.eatToken(.comma)) |_| {
        const next: TokenIndex = p.eatToken(.variable) orelse return p.warn(.expected_variable);
        try p.scratch.append(p.gpa, next);
    }

    return p.moveScratchToExtra(p.scratch.items[top..]);
}

fn expectExpr(p: *Parse) Error!Node.Index {
    var expr = switch (p.tokenTag(p.tok_i)) {
        .minus => try p.addNode(.{
            .tag = .negate,
            .main_token = p.nextToken(),
            .data = .{ .node = try p.expectTerm() },
        }),
        .plus => try p.addNode(.{
            .tag = .identity,
            .main_token = p.nextToken(),
            .data = .{ .node = try p.expectTerm() },
        }),
        else => try p.expectTerm(),
    };

    while (true) {
        switch (p.tokenTag(p.tok_i)) {
            .minus => expr = try p.addNode(.{
                .tag = .sub,
                .main_token = p.nextToken(),
                .data = .{ .node_and_node = .{ expr, try p.expectTerm() } },
            }),
            .plus => expr = try p.addNode(.{
                .tag = .add,
                .main_token = p.nextToken(),
                .data = .{ .node_and_node = .{ expr, try p.expectTerm() } },
            }),
            else => break,
        }
    }

    return expr;
}

fn expectTerm(p: *Parse) Error!Node.Index {
    var term = try p.expectFactor();

    while (true) {
        switch (p.tokenTag(p.tok_i)) {
            .star => term = try p.addNode(.{
                .tag = .mul,
                .main_token = p.nextToken(),
                .data = .{ .node_and_node = .{ term, try p.expectTerm() } },
            }),
            .slash => term = try p.addNode(.{
                .tag = .div,
                .main_token = p.nextToken(),
                .data = .{ .node_and_node = .{ term, try p.expectTerm() } },
            }),
            else => break,
        }
    }

    return term;
}

fn expectFactor(p: *Parse) Error!Node.Index {
    switch (p.tokenTag(p.tok_i)) {
        .variable => return p.addNode(.{
            .tag = .variable,
            .main_token = p.nextToken(),
            .data = undefined,
        }),
        .number_literal => {
            const number = p.nextToken();

            var scanner: Scanner = .{
                .src = @ptrCast(p.source),
                .index = p.tokenStart(number),
            };
            const loc = scanner.next().loc;
            // TODO: Use warning here.
            _ = std.fmt.parseInt(usize, p.source[loc.start..loc.end], 10) catch |err| switch (err) {
                error.Overflow => try p.errors.append(p.gpa, .{
                    .tag = .overflow,
                    .token = number,
                    .extra = .{ .none = {} },
                }),
                error.InvalidCharacter => unreachable,
            };

            return p.addNode(.{
                .tag = .number_literal,
                .main_token = number,
                .data = undefined,
            });
        },
        else => return p.expectGrouping(),
    }
}

fn expectGrouping(p: *Parse) Error!Node.Index {
    if (p.tokenTag(p.tok_i) != .l_paren)
        return p.warn(.expected_grouping);
    const l_paren = p.nextToken();
    const inner = try p.expectExpr();
    if (p.tokenTag(p.tok_i) != .r_paren)
        return p.warn(.expected_grouping);
    const r_paren = p.nextToken();
    return p.addNode(.{
        .tag = .group,
        .main_token = l_paren,
        .data = .{ .node_and_token = .{ inner, r_paren } },
    });
}

fn expectCondition(p: *Parse) Error!Node.Index {
    var node: Node = undefined;
    const lhs = try p.expectExpr();
    switch (p.tokenTag(p.tok_i)) {
        .less => node.tag = .less,
        .less_equal => node.tag = .less_equal,
        .less_greater => node.tag = .not_equal,
        .greater => node.tag = .greater,
        .greater_equal => node.tag = .greater_equal,
        .greater_less => node.tag = .not_equal,
        .equal => node.tag = .equal,
        else => return p.warn(.expected_relop),
    }
    node.main_token = p.nextToken();
    const rhs = try p.expectExpr();
    node.data = .{ .node_and_node = .{ lhs, rhs } };
    return p.addNode(node);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const AstError = Ast.Error;
const TokenList = Ast.TokenList;
const TokenIndex = Ast.TokenIndex;
const Token = @import("Token.zig");
const Parse = @This();
const Scanner = Token.Scanner;
