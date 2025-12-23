//! src/ast.zig

pub const TokenKind = enum {
    left_paren,
    right_paren,
    comma,
    minus,
    plus,
    slash,
    star,
    equal,
    greater,
    greater_equal,
    greater_less,
    less,
    less_equal,
    less_greater,
    print,
    @"if",
    then,
    goto,
    input,
    let,
    gosub,
    @"return",
    clear,
    list,
    run,
    end,
    eol,
    eof,
    @"var",
    number,
    string,
};

pub const Token = union(TokenKind) {
    left_paren,
    right_paren,
    comma,
    minus,
    plus,
    slash,
    star,
    equal,
    greater,
    greater_equal,
    greater_less,
    less,
    less_equal,
    less_greater,
    print,
    @"if",
    then,
    goto,
    input,
    let,
    gosub,
    @"return",
    clear,
    list,
    run,
    end,
    eol,
    eof: void,
    @"var": u8,
    number: usize,
    string: []const u8,

    pub fn valued_literal(self: Token) ?ValuedLiteral {
        return switch (self) {
            .@"var" => |v| .{ .@"var" = v },
            .number => |v| .{ .number = v },
            else => null,
        };
    }

    pub fn relop(self: Token) ?Relop {
        return switch (self) {
            .less => .less,
            .less_equal => .less_equal,
            .less_greater => .less_greater,
            .greater => .greater,
            .greater_equal => .greater_equal,
            .greater_less => .greater_less,
            .equal => .equal,
            else => null,
        };
    }

    pub fn op(self: Token) ?Operator {
        return switch (self) {
            .plus => .plus,
            .minus => .minus,
            .star => .mul,
            .slash => .div,
            else => null,
        };
    }
};

pub const ValuedLiteral = union(enum) {
    @"var": u8,
    number: usize,
    string: []const u8,
};

pub const Relop = enum {
    less,
    less_equal,
    less_greater,
    greater,
    greater_equal,
    greater_less,
    equal,
};

pub const Operator = enum {
    plus,
    minus,
    div,
    mul,
};

/// Root statement of each line.
pub const Root = struct {
    line: ?usize,
    stmt: Stmt,
};

/// An expression with one argument to the right of the operator.
pub const UnaryExpr = struct {
    op: Operator,
    rhs: Expr,
};

/// An expression with arguments to the left and right of the operator.
pub const BinaryExpr = struct {
    op: Operator,
    lhs: Expr,
    rhs: Expr,
};

/// Any expression
pub const Expr = union(enum) {
    unary: *UnaryExpr,
    binary: *BinaryExpr,
    literal: ValuedLiteral,
    grouping: *Expr,
};

/// A linked list of variables.
pub const VarList = struct {
    @"var": u8,
    next: ?*VarList,
};

pub const ExprListValue = union(enum) {
    string: []const u8,
    expr: Expr,
};

/// A linked list of expression values.
pub const ExprList = struct {
    expr: ExprListValue,
    next: ?*ExprList,
};

/// Any statement
pub const Stmt = union(enum) {
    print: ExprList,
    @"if": *IfStmt,
    input: VarList,
    let: LetStmt,
    goto: Expr,
    gosub: Expr,
    @"return",
    clear,
    list,
    run,
    end,
};

pub const IfStmt = struct {
    lhs_expr: Expr,
    relop: Relop,
    rhs_expr: Expr,
    stmt: Stmt,
};

pub const LetStmt = struct {
    @"var": u8,
    expr: Expr,
};

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
