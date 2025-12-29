//! src/Ast.zig
//! an Abstract Syntax Tree (AST) implemented using nodes in a list
//! The advantage this implementation has over using an arena and
//! forgetting about it, is that it allows for lists eg in the
//! case of the print and input statements which are variadic
//! in their arguments.

// So bloody complex..

/// Reference to externally-owned data.
source: [:0]const u8,

mode: Mode,
tokens: TokenList.Slice,
nodes: NodeList.Slice,
extra_data: []const u32,
errors: []const Error,

pub const ByteOffset = u32;

pub const TokenList = std.MultiArrayList(struct {
    tag: Token.Tag,
    start: ByteOffset,
});

pub const TokenIndex = u32;

pub const NodeList = std.MultiArrayList(Node);

pub const ExtraIndex = enum(u32) { _ };

pub const Node = struct {
    tag: Tag,
    main_token: TokenIndex,
    data: Data,

    pub const Tag = enum {
        /// The root node. Must exist at `Node.Index.root`.
        ///
        /// The `data` field is a `.extra_range`.
        /// Where each element in the range is a `Node.Index`.
        ///
        /// The `main_token` is the first token in the file.
        root,
        /// The `data` field is unused.
        string_literal,
        /// The `data` field is unused.
        number_literal,
        /// The `data` field is unused.
        variable,
        /// `(expr)`.
        ///
        /// The `data` field is `node_and_token`.
        ///  1. a `Node.Index` to the sub-expression.
        ///  2. a `TokenIndex` to the `)` token.
        ///
        /// The `main_token` field is the `(` token.
        group,
        /// `lhs + rhs`.
        ///
        /// The `main_token` field is the `+` token.
        add,
        /// `lhs - rhs`.
        ///
        /// The `main_token` field is the `-` token.
        sub,
        /// `lhs * rhs`.
        ///
        /// The `main_token` field is the `*` token.
        mul,
        /// `lhs / rhs`.
        ///
        /// The `main_token` field is the `/` token.
        div,
        /// `+rhs`.
        /// The `main_token` field is the `+` token.
        identity,
        /// `-rhs`.
        /// The `main_token` field is the `-` token.
        negate,
        /// `lhs < rhs`.
        ///
        /// The `main_token` field is the `<` token.
        less,
        /// `lhs <= rhs`.
        ///
        /// The `main_token` field is the `<=` token.
        less_equal,
        /// `lhs > rhs`.
        ///
        /// The `main_token` field is the `>` token.
        greater,
        /// `lhs >= rhs`.
        ///
        /// The `main_token` field is the `>=` token.
        greater_equal,
        /// `lhs == rhs`.
        ///
        /// The `main_token` field is the `=` token.
        equal,
        /// `lhs != rhs`.
        ///
        /// The `main_token` field is either the `<>` or `><` tokens.
        not_equal,
        /// `line stmt`.
        ///
        /// The `data` field is a '.node'.
        ///   a `Node.Index` to the deffered statement.
        ///
        /// The `main_token` field is the number_literal token.
        defered_statement,
        /// `PRINT "Hello", X, ...`.
        ///
        /// The `data` field is a `.extra_range`.
        /// Where each element in the range is a `TokenIndex`.
        ///
        /// The `main_token` field is the `PRINT` token.
        print,
        /// `IF expr relop expr THEN stmt`.
        ///
        /// The `data` field is a `node_and_node`:
        ///   1. a `Node.Index` to the boolean statement.
        ///   2. a `Node.Index` to the conditional statement.
        ///
        /// The `main_token` field is the `IF` token.
        @"if",
        /// `GOTO expr`.
        ///
        /// The `data` field is a `.node`.
        ///
        /// The `main_token` is the `GOTO` token.
        goto,
        /// `INPUT X, ...`.
        ///
        /// The `data` field is a `.extra_range`.
        /// Where each element in the range is a `TokenIndex`.
        ///
        /// The `main_token` is the `INPUT` token.
        input,
        /// `LET var = expr`.
        ///
        /// The `data` field is a `.node_and_token`:
        ///   1. a `Node.Index` to the expression value.
        ///   2. a `TokenIndex` to the target variable.
        ///
        /// The `main_token` is the `LET` token.
        let,
        /// `GOSUB expr`.
        ///
        /// The `data` field is a `.node`:
        ///   1. a `Node.Index` to the expression value
        ///
        /// The `main_token` is the `GOSUB` token.
        gosub,
        /// `RETURN`.
        /// The `data` field is unused.
        /// The `main_token` field is the `RETURN` token.
        @"return",
        /// `CLEAR`.
        /// The `data` field is unused.
        /// The `main_token` field is the `CLEAR` token.
        clear,
        /// `LIST`.
        /// The `data` field is unused.
        /// The `main_token` field is the `LIST` token.
        list,
        /// `RUN`.
        /// The `data` field is unused.
        /// The `main_token` field is the `RUN` token.
        run,
        /// `END`.
        /// The `data` field is unused.
        /// The `main_token` field is the `END` token.
        end,
    };

    pub const Data = union(enum) {
        node: Node.Index,
        token: TokenIndex,
        node_and_node: struct { Node.Index, Node.Index },
        node_and_token: struct { Node.Index, TokenIndex },
        token_and_token: struct { TokenIndex, TokenIndex },
        extra_range: SubRange,
    };

    pub const Index = enum(u32) {
        root = 0,
        _,
    };

    pub const SubRange = struct {
        /// Index into `extra_data`.
        start: ExtraIndex,
        /// Index into `extra_data`.
        end: ExtraIndex,

        pub fn len(r: SubRange) usize {
            return @intFromEnum(r.end) - @intFromEnum(r.start);
        }

        pub fn slice(r: SubRange, extra: []const u32) []const u32 {
            return extra[@intFromEnum(r.start)..@intFromEnum(r.end)];
        }
    };
};

// TODO: Fix tag + extra.
//       Use properly in code base.
//       Make src/Fmt.zig able to print errors.
pub const Error = struct {
    tag: Tag,
    token: TokenIndex,
    extra: union(enum) {
        expected_tag: Token.Tag,
        none: void,
    },

    pub const Tag = enum {
        expected_line_number_or_stmt,
        expected_stmt,
        expected_expr,
        expected_relop,
        expected_var_list,
        expected_expr_list,
        expected_grouping,
        expected_then,
        expected_variable,
        expected_equal,
        expected_eof,
        overflow,
        zero_line_number,
    };
};

pub fn deinit(tree: *Ast, gpa: Allocator) void {
    tree.tokens.deinit(gpa);
    tree.nodes.deinit(gpa);
    gpa.free(tree.extra_data);
    gpa.free(tree.errors);
    tree.* = undefined;
}

pub const Mode = enum {
    chunked,
    all,
};

pub fn parse(gpa: Allocator, source: [:0]const u8, mode: Mode) Allocator.Error!Ast {
    var tokens: TokenList = .empty;
    defer tokens.deinit(gpa);

    var scanner: Token.Scanner = .{ .src = source };
    while (true) {
        const token = scanner.next();
        try tokens.append(gpa, .{
            .tag = token.tag,
            .start = @intCast(token.loc.start),
        });
        if (token.tag == .eof) break;
    }

    var nodes: NodeList = .empty;
    try nodes.ensureTotalCapacity(gpa, 1);

    var parser: Parse = .{
        .gpa = gpa,
        .source = source,
        .tokens = tokens.slice(),
        .tok_i = 0,
        .errors = .empty,
        .nodes = nodes,
        .extra_data = .empty,
        .scratch = .empty,
    };
    defer parser.errors.deinit(gpa);
    defer parser.nodes.deinit(gpa);
    defer parser.extra_data.deinit(gpa);
    defer parser.scratch.deinit(gpa);

    switch (mode) {
        .chunked => try parser.parseChunk(),
        .all => try parser.parseRoot(),
    }

    const extra_data = try parser.extra_data.toOwnedSlice(gpa);
    errdefer gpa.free(extra_data);

    const errors = try parser.errors.toOwnedSlice(gpa);
    errdefer gpa.free(errors);

    return .{
        .source = source,
        .tokens = tokens.toOwnedSlice(),
        .nodes = parser.nodes.toOwnedSlice(),
        .extra_data = extra_data,
        .errors = errors,
        .mode = mode,
    };
}

const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const Allocator = std.mem.Allocator;
const Ast = @This();
const Token = @import("Token.zig");
const Parse = @import("Parse.zig");
