//! src/ast.zig
//! Contains all of the AST implementation for the Tiny Basic language

/// The types of tokens we can have:
/// TODO: Rename to TokenKind
pub const TokenTag = enum {
    // Single character tokens
    left_paren,
    right_paren,
    comma,
    minus,
    plus,
    slash,
    star,
    @"var",
    equal,

    // One or two character tokens
    greater,
    greater_equal,
    less,
    less_equal,
    less_greater,

    // Literals
    string,
    number,

    // Keywords
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

    // Natural Seperations (Tiny Basic works on a line-by-line basis so this works)
    eol,
    eof,
};

/// Literal types
/// as a hack variables are also literals.
pub const LiteralType = enum {
    number,
    string,
    @"var",
};

/// The literal values.
pub const Literal = union(LiteralType) {
    number: u32,
    string: []const u8,
    // For convenience we can pretend that a variable is a literal
    @"var": u8,
};

/// Extremely simple token: line, tag and literal.
pub const Token = struct {
    // Line of the token.
    line: usize,

    // The tag for the token
    tag: TokenTag,

    // Literal data
    literal: ?Literal,
};

/// Root statement of each line.
pub const Root = struct {
    line: ?Token,
    stmt: Stmt,
};

/// An expression with one argument to the right of the operator.
pub const UnaryExpr = struct {
    op: Token,
    rhs: Expr,
};

/// An expression with arguments to the left and right of the operator.
pub const BinaryExpr = struct {
    op: Token,
    lhs: Expr,
    rhs: Expr,
};

/// Possible kinds of expressions
pub const ExprTag = enum {
    unary,
    binary,
    literal,
    grouping,
};

/// Any expression
pub const Expr = union(ExprTag) {
    unary: *UnaryExpr,
    binary: *BinaryExpr,
    literal: Token,
    grouping: *Expr,
};

/// A linked list of variables.
pub const VarList = struct {
    @"var": Token,
    next: ?*VarList,
};

pub const ExprListValueTag = enum { string, expr };

pub const ExprListValue = union(ExprListValueTag) {
    string: Token,
    expr: Expr,
};

/// A linked list of expression values.
pub const ExprList = struct {
    expr: ExprListValue,
    next: ?*ExprList,
};

pub const StmtTag = enum {
    print,
    @"if",
    goto,
    input,
    let,
    gosub,
    @"return",
    clear,
    list,
    run,
    end,
};

/// Any statement
pub const Stmt = union(StmtTag) {
    print: ExprList,
    @"if": *IfStmt,
    goto: Expr,
    input: VarList,
    let: LetStmt,
    gosub: Expr,
    @"return": void,
    clear: void,
    list: void,
    run: void,
    end: void,
};

pub const IfStmt = struct {
    lhs_expr: Expr,
    relop: Token,
    rhs_expr: Expr,
    stmt: Stmt,
};

pub const LetStmt = struct {
    @"var": Token,
    expr: Expr,
};

const std = @import("std");
const assert = std.debug.assert;
const testing = std.testing;
