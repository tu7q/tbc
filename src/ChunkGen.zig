//! src/ChunkGen.zig
//! Used to generate Tiny Basic Intermediate Representation

gpa: Allocator,
tree: *const Ast,
line_number: Chunk.LineNumber,
instructions: std.ArrayListUnmanaged(Chunk.Inst),
variables: std.ArrayListUnmanaged(u8),
numbers: std.ArrayListUnmanaged(usize),
strings: std.ArrayListUnmanaged([]const u8),

pub fn generate(cg: *ChunkGen) Allocator.Error!void {
    assert(cg.tree.errors.len == 0);
    assert(cg.tree.mode == .chunked);

    const root = cg.tree.nodes.get(@intFromEnum(Node.Index.root));
    assert(root.tag == .root);

    try cg.generateFromStmt(root.data.node);
}

/// Generates tbir chunk from a top level statement.
pub fn generateFromStmt(cg: *ChunkGen, node_index: Node.Index) Allocator.Error!void {
    assert(cg.tree.errors.len == 0);

    const node = cg.tree.nodes.get(@intFromEnum(node_index));

    var stmt_index: Node.Index = undefined;
    if (node.tag == .defered_statement) {
        cg.line_number =
            @enumFromInt(cg.resolveNumber(node.main_token));
        stmt_index = node.data.node;
    } else {
        cg.line_number = .immediate;
        stmt_index = node_index;
    }

    try cg.generateStmt(stmt_index);
}

fn generateStmt(cg: *ChunkGen, index: Node.Index) Allocator.Error!void {
    const node = cg.tree.nodes.get(@intFromEnum(index));
    switch (node.tag) {
        .print => try cg.generatePrintList(node.data.extra_range),
        .@"if" => {
            const nan = node.data.node_and_node;
            try cg.generateComparison(nan.@"0");
            try cg.instructions.append(cg.gpa, .break_if_not_cmp);
            try cg.generateStmt(nan.@"1");
        },
        .goto => {
            try cg.generateExpression(node.data.node);
            try cg.instructions.append(cg.gpa, .goto);
        },
        .input => try cg.generateInputList(node.data.extra_range),
        .let => {
            const nat = node.data.node_and_token;
            try cg.generateExpression(nat.@"0");
            try cg.variables.append(cg.gpa, cg.resolveVariable(nat.@"1"));
            try cg.instructions.append(cg.gpa, .assign);
        },
        .gosub => {
            try cg.generateExpression(node.data.node);
            try cg.instructions.append(cg.gpa, .gosub);
        },
        .@"return" => try cg.instructions.append(cg.gpa, .@"return"),
        .clear => try cg.instructions.append(cg.gpa, .clear),
        .list => try cg.instructions.append(cg.gpa, .list),
        .run => try cg.instructions.append(cg.gpa, .run),
        .end => try cg.instructions.append(cg.gpa, .end),
        else => unreachable,
    }
}

fn generatePrintList(cg: *ChunkGen, range: Node.SubRange) Allocator.Error!void {
    var first: bool = true;

    for (range.slice(cg.tree.extra_data)) |i| {
        if (!first)
            try cg.instructions.append(cg.gpa, .print_delimiter);

        const node = cg.tree.nodes.get(i);
        switch (node.tag) {
            .string_literal => {
                try cg.strings.append(cg.gpa, cg.resolveString(node.main_token));
                try cg.instructions.append(cg.gpa, .print_str);
            },
            else => {
                try cg.generateExpression(@enumFromInt(i));
                try cg.instructions.append(cg.gpa, .print_num);
            },
        }

        first = false;
    }

    try cg.instructions.append(cg.gpa, .print_flush);
}

fn generateInputList(cg: *ChunkGen, range: Node.SubRange) Allocator.Error!void {
    try cg.instructions.append(cg.gpa, .read_line);

    for (range.slice(cg.tree.extra_data)) |i| {
        try cg.variables.append(cg.gpa, cg.resolveVariable(i));
        try cg.instructions.append(cg.gpa, .push_val);
    }
}

fn generateComparison(cg: *ChunkGen, index: Ast.Node.Index) Allocator.Error!void {
    const node = cg.tree.nodes.get(@intFromEnum(index));
    const nan = node.data.node_and_node;
    try cg.generateExpression(nan.@"0");
    try cg.generateExpression(nan.@"1");
    switch (node.tag) {
        .less => try cg.instructions.append(cg.gpa, .cmp_lt),
        .less_equal => try cg.instructions.append(cg.gpa, .cmp_lte),
        .greater => try cg.instructions.append(cg.gpa, .cmp_gt),
        .greater_equal => try cg.instructions.append(cg.gpa, .cmp_gte),
        .equal => try cg.instructions.append(cg.gpa, .cmp_eq),
        .not_equal => try cg.instructions.append(cg.gpa, .cmp_ne),
        else => unreachable,
    }
}

fn generateExpression(cg: *ChunkGen, index: Ast.Node.Index) Allocator.Error!void {
    const node = cg.tree.nodes.get(@intFromEnum(index));
    switch (node.tag) {
        .number_literal => {
            try cg.numbers.append(cg.gpa, cg.resolveNumber(node.main_token));
            try cg.instructions.append(cg.gpa, .load_number);
        },
        .variable => {
            try cg.variables.append(cg.gpa, cg.resolveVariable(node.main_token));
            try cg.instructions.append(cg.gpa, .load_variable);
        },
        .negate => {
            try cg.generateExpression(node.data.node);
            try cg.instructions.append(cg.gpa, .negate);
        },
        .identity => try cg.generateExpression(node.data.node),
        .add, .sub, .mul, .div => {
            const nan = node.data.node_and_node;
            try cg.generateExpression(nan.@"0");
            try cg.generateExpression(nan.@"1");
            try cg.instructions.append(cg.gpa, switch (node.tag) {
                .add => .add,
                .sub => .sub,
                .mul => .mul,
                .div => .div,
                else => unreachable,
            });
        },
        .group => try cg.generateExpression(node.data.node_and_token.@"0"),
        else => unreachable,
    }
}
fn scanToken(tree: *const Ast, token_index: Ast.TokenIndex) []const u8 {
    var scanner: Scanner = .{
        .src = @ptrCast(tree.source),
        .index = tree.tokens.items(.start)[token_index],
    };
    const token = scanner.next();
    return tree.source[token.loc.start..token.loc.end];
}

fn resolveVariable(cg: *const ChunkGen, token_index: Ast.TokenIndex) u8 {
    assert(cg.tree.tokens.items(.tag)[token_index] == .variable);
    const str = scanToken(cg.tree, token_index);
    assert(str.len == 1);

    return str[0];
}

fn resolveNumber(cg: *const ChunkGen, token_index: Ast.TokenIndex) usize {
    assert(cg.tree.tokens.items(.tag)[token_index] == .number_literal);

    const str = scanToken(cg.tree, token_index);
    // This is safe because all number literals have been
    // sanitized first by the scanner and then by the parser
    return std.fmt.parseInt(usize, str, 10) catch unreachable;
}

fn resolveString(cg: *const ChunkGen, token_index: Ast.TokenIndex) []const u8 {
    const str = scanToken(cg.tree, token_index);
    return str[1 .. str.len - 1];
}

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const ChunkGen = @This();
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Chunk = @import("Chunk.zig");
const Scanner = @import("Token.zig").Scanner;
