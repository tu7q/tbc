//! src/fmt.zig

///
pub fn prettyPrint(root: ast.Root, writer: *Io.Writer) !void {
    if (root.line) |line| {
        try writer.print("{d} ", .{line});
    }

    try ppStmt(root.stmt, writer);
    try writer.writeAll("\n");
}

///
pub fn ppUnaryExpr(unary_expr: *ast.UnaryExpr, writer: *Io.Writer) anyerror!void {
    const lexeme = switch (unary_expr.op) {
        .plus => "+",
        .minus => "-",
        else => @panic("token was not a unary operator"),
    };

    try writer.print("{s} ", .{lexeme});
    try ppExpr(unary_expr.rhs, writer);
}

///
pub fn ppBinaryExpr(binaray_expr: *ast.BinaryExpr, writer: *Io.Writer) !void {
    const lexeme = switch (binaray_expr.op) {
        .div => "/",
        .mul => "*",
        .minus => "-",
        .plus => "+",
    };

    try ppExpr(binaray_expr.lhs, writer);
    try writer.print(" {s} ", .{lexeme});
    try ppExpr(binaray_expr.rhs, writer);
}

///
pub fn ppLiteralExpr(literal: ast.ValuedLiteral, writer: *Io.Writer) anyerror!void {
    switch (literal) {
        .@"var" => |v| try writer.print("{c}", .{v}),
        .number => |n| try writer.print("{d}", .{n}),
        .string => |s| try writer.print("{s}", .{s}),
    }
}

///
pub fn ppGroupingExpr(group: *ast.Expr, writer: *Io.Writer) anyerror!void {
    try writer.writeAll("(");
    try ppExpr(group.*, writer);
    try writer.writeAll("(");
}

///
pub fn ppExpr(expr: ast.Expr, writer: *Io.Writer) anyerror!void {
    switch (expr) {
        .unary => |unary| try ppUnaryExpr(unary, writer),
        .binary => |binary| try ppBinaryExpr(binary, writer),
        .literal => |literal| try ppLiteralExpr(literal, writer),
        .grouping => |group| try ppGroupingExpr(group, writer),
    }
}

///
pub fn ppExprList(list: ast.ExprList, writer: *Io.Writer) !void {
    switch (list.expr) {
        .expr => |expr| try ppExpr(expr, writer),
        .string => |s| try writer.print("\"{s}\"", .{s}),
    }

    if (list.next) |next| {
        try writer.writeAll(", ");
        try ppExprList(next.*, writer);
    }
}

///
pub fn ppVarList(list: ast.VarList, writer: *Io.Writer) !void {
    try writer.print("{c}", .{list.@"var"});
    if (list.next) |next| {
        try writer.writeAll(", ");
        try ppVarList(next.*, writer);
    }
}

///
pub fn ppRelop(relop: ast.Relop, writer: *Io.Writer) !void {
    const lexeme = switch (relop) {
        .less => "<",
        .less_equal => "<=",
        .less_greater => "<>",
        .greater => ">",
        .greater_equal => ">=",
        .greater_less => "><",
        .equal => "=",
    };

    try writer.print(" {s} ", .{lexeme});
}

///
pub fn ppStmt(stmt: ast.Stmt, writer: *Io.Writer) !void {
    switch (stmt) {
        .print => |list| {
            try writer.writeAll("PRINT ");
            try ppExprList(list, writer);
        },
        .@"if" => |@"if"| {
            try writer.writeAll("IF ");
            try ppExpr(@"if".lhs_expr, writer);
            try ppRelop(@"if".relop, writer);
            try ppExpr(@"if".rhs_expr, writer);
            try writer.writeAll(" THEN ");
            try ppStmt(@"if".stmt, writer);
        },
        .goto => |expr| {
            try writer.writeAll("GOTO ");
            try ppExpr(expr, writer);
        },
        .input => |list| {
            try writer.writeAll("INPUT ");
            try ppVarList(list, writer);
        },
        .let => |let| {
            try writer.writeAll("LET ");
            try writer.print("{c} = ", .{let.@"var"});
            try ppExpr(let.expr, writer);
        },
        .gosub => |expr| {
            try writer.writeAll("GOSUB ");
            try ppExpr(expr, writer);
        },
        .@"return" => try writer.writeAll("RETURN"),
        .clear => try writer.writeAll("CLEAR"),
        .list => try writer.writeAll("LIST"),
        .run => try writer.writeAll("RUN"),
        .end => try writer.writeAll("END"),
    }
}

const std = @import("std");
const assert = std.debug.assert;

const Io = std.Io;

const ast = @import("ast.zig");
