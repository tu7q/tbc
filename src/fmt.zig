//! src/Fmt.zig
//! TODO: Re-order defered stmts by line.
//!       (no immediate mode sttms allowed)

tree: Ast,
w: *Writer,

pub fn printTreeToFile(file: std.fs.File, tree: Ast) Writer.Error!void {
    var buf: [1024]u8 = undefined;
    var fwriter = file.writer(&buf);

    const fmt: Fmt = .{
        .tree = tree,
        .w = &fwriter.interface,
    };

    try fmt.printTree();
    try (&fwriter.interface).flush();
}

pub fn printTree(fmt: *const Fmt) Writer.Error!void {
    // Can't format a tree with no errors.
    assert(fmt.tree.errors.len == 0);

    const root = fmt.tree.nodes.get(@intFromEnum(Ast.Node.Index.root));
    assert(root.tag == .root);

    const range = root.data.extra_range;
    for (range.slice(fmt.tree.extra_data)) |i| {
        try fmt.printTopLevelStmt(@enumFromInt(i));
    }
}

pub fn printTopLevelStmt(fmt: *const Fmt, index: NodeIndex) Writer.Error!void {
    const node = fmt.tree.nodes.get(@intFromEnum(index));
    switch (node.tag) {
        .defered_statement => {
            try fmt.w.print("{s} ", .{fmt.scanToken(node.main_token)});
            try fmt.printStmt(node.data.node);
        },
        else => try fmt.printStmt(index),
    }
    try fmt.w.writeAll("\n");
}

fn printStmt(fmt: *const Fmt, index: NodeIndex) Writer.Error!void {
    const node = fmt.tree.nodes.get(@intFromEnum(index));
    switch (node.tag) {
        .print => {
            try fmt.w.writeAll("PRINT ");
            try fmt.printExprList(node.data.extra_range);
        },
        .@"if" => {
            const node_and_node = node.data.node_and_node;
            try fmt.w.writeAll("IF ");
            try fmt.printComparison(node_and_node.@"0");
            try fmt.w.writeAll(" THEN ");
            try fmt.printStmt(node_and_node.@"1");
        },
        .goto => {
            try fmt.w.writeAll("GOTO ");
            try fmt.printExpr(node.data.node);
        },
        .input => {
            try fmt.w.writeAll("INPUT ");
            try fmt.printVarList(node.data.extra_range);
        },
        .let => {
            const node_and_token = node.data.node_and_token;
            try fmt.w.print("LET {s} = ", .{fmt.scanToken(node_and_token.@"1")});
            try fmt.printExpr(node_and_token.@"0");
        },
        .gosub => {
            try fmt.w.writeAll("GOTO ");
            try fmt.printExpr(node.data.node);
        },
        .@"return" => try fmt.w.writeAll("RETURN"),
        .clear => try fmt.w.writeAll("CLEAR"),
        .list => try fmt.w.writeAll("LIST"),
        .run => try fmt.w.writeAll("RUN"),
        .end => try fmt.w.writeAll("END"),
        else => unreachable,
    }
}

fn printExprList(fmt: *const Fmt, range: Ast.Node.SubRange) Writer.Error!void {
    var pad: []const u8 = "";

    for (range.slice(fmt.tree.extra_data)) |i| {
        const node = fmt.tree.nodes.get(i);

        try fmt.w.writeAll(pad);

        switch (node.tag) {
            .string_literal => {
                try fmt.w.print("{s}", .{fmt.scanToken(node.main_token)});
            },
            else => try fmt.printExpr(@enumFromInt(i)),
        }

        pad = ", ";
    }
}

fn printVarList(fmt: *const Fmt, range: Ast.Node.SubRange) Writer.Error!void {
    var pad: []const u8 = "";
    for (range.slice(fmt.tree.extra_data)) |i| {
        try fmt.w.writeAll(pad);

        try fmt.w.print("{s}", .{fmt.scanToken(i)});

        pad = ", ";
    }
}

fn printComparison(fmt: *const Fmt, index: NodeIndex) Writer.Error!void {
    const node = fmt.tree.nodes.get(@intFromEnum(index));

    try fmt.printExpr(node.data.node_and_node.@"0");
    try fmt.w.print(" {s} ", .{fmt.scanToken(node.main_token)});
    try fmt.printExpr(node.data.node_and_node.@"1");
}

fn printExpr(fmt: *const Fmt, index: NodeIndex) Writer.Error!void {
    const node = fmt.tree.nodes.get(@intFromEnum(index));
    switch (node.tag) {
        .number_literal,
        .variable,
        => try fmt.w.print("{s}", .{fmt.scanToken(node.main_token)}),
        .negate, .identity => {
            try fmt.w.print("{s}", .{fmt.scanToken(node.main_token)});
            try fmt.printExpr(node.data.node);
        },
        .add,
        .sub,
        .mul,
        .div,
        => {
            try fmt.printExpr(node.data.node_and_node.@"0");
            try fmt.w.print(" {s} ", .{fmt.scanToken(node.main_token)});
            try fmt.printExpr(node.data.node_and_node.@"1");
        },
        .group => {
            try fmt.w.writeAll("(");
            try fmt.printExpr(node.data.node_and_token.@"0");
            try fmt.w.writeAll(")");
        },
        else => unreachable,
    }
}

fn scanToken(fmt: *const Fmt, token_index: Ast.TokenIndex) []const u8 {
    var scanner: Scanner = .{
        .src = @ptrCast(fmt.tree.source),
        .index = fmt.tree.tokens.items(.start)[token_index],
    };
    const token = scanner.next();

    return fmt.tree.source[token.loc.start..token.loc.end];
}

const std = @import("std");
const assert = std.debug.assert;

const Io = std.Io;
const Writer = Io.Writer;

const Ast = @import("Ast.zig");
const NodeIndex = Ast.Node.Index;
const Scanner = @import("Token.zig").Scanner;
const Fmt = @This();
