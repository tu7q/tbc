//! src/Chunk.zig

line: LineNumber,
instructions: []const Inst,
src: []const u8,
variables: []const u8,
numbers: []const usize,
strings: []const []const u8,

pub const LineNumber = enum(usize) {
    immediate = 0,
    _,
};

pub const Inst = enum(u8) {
    /// pop a, b from value stack
    /// sets cmp flag to a < b
    cmp_lt,
    /// pop a, b from value stack
    /// sets cmp flag to a <= b
    cmp_lte,
    /// pop a, b from value stack
    /// sets cmp flag to a > b
    cmp_gt,
    /// pop a, b from value stack
    /// sets cmp flag to a >= b
    cmp_gte,
    /// pop a, b from value stack
    /// sets cmp flag to a == b
    cmp_eq,
    /// pop a, b from value stack
    /// sets cmp flag to a != b
    cmp_ne,
    /// read num from number list
    /// push num to value stack
    load_number,
    /// read var from variable list
    /// push var to value stack
    load_variable,
    /// pop a, b from value stack
    /// push a + b to value stack
    add,
    /// pop a, b from value stack
    /// push a - b to value stack
    sub,
    /// pop a, b from value stack
    /// push a * b to value stack
    mul,
    /// pop a, b from value stack
    /// push a / b to value stack
    div,
    /// pop a from value stack
    /// push -a to value stack
    negate,
    /// pop a from value stack
    /// print the num
    print_num,
    /// read str from string list
    /// print the str
    print_str,
    /// prints the delimiter characters
    print_delimiter,
    /// prints the flush characters
    print_flush,
    /// If the cmp flag is enabled
    /// then it breaks the current line
    break_if_not_cmp,
    /// pop a from value stack
    /// move execution to line a
    goto,
    /// pop a from value stack
    /// push current line to return stack
    /// move execution to line a
    gosub,
    /// read from stdin into the line register.
    read_line,
    /// push next parsed value from line into value register
    push_val,
    /// pop a from value stack
    /// read var from var list
    /// sets var to a
    assign,
    /// pop a from return stack
    /// move execution to line return
    @"return",
    /// clear the terminal
    clear,
    /// print all lines
    list,
    /// move execution to first line
    run,
    /// halt execution
    end,
};

pub fn deinit(chunk: *Chunk, gpa: Allocator) void {
    gpa.free(chunk.instructions);
    gpa.free(chunk.variables);
    gpa.free(chunk.numbers);
    gpa.free(chunk.strings);
    chunk.* = undefined;
}

pub const Iterator = struct {
    gen: ChunkGen,
    stmts: []const Ast.Node.Index,
    node_i: usize,

    pub fn deinit(it: *Iterator) void {
        it.gen.instructions.deinit(it.gen.gpa);
        it.gen.variables.deinit(it.gen.gpa);
        it.gen.numbers.deinit(it.gen.gpa);
        it.gen.strings.deinit(it.gen.gpa);
        it.* = undefined;
    }

    pub fn init(gpa: Allocator, tree: *const Ast) Iterator {
        assert(tree.errors.len == 0);
        assert(tree.mode == .all);

        const root = tree.nodes.get(@intFromEnum(Ast.Node.Index.root));
        assert(root.tag == .root);

        const range = root.data.extra_range;
        const stmts = range.slice(tree.extra_data);

        return .{
            .node_i = 0,
            .stmts = @ptrCast(stmts),
            .gen = .{
                .gpa = gpa,
                .tree = tree,
                .line_number = undefined,
                .instructions = .empty,
                .variables = .empty,
                .numbers = .empty,
                .strings = .empty,
            },
        };
    }

    pub fn next(it: *Iterator, gpa: Allocator) Allocator.Error!?Chunk {
        if (it.node_i >= it.stmts.len) return null;

        it.gen.line_number = undefined;
        it.gen.instructions.clearRetainingCapacity();
        it.gen.variables.clearRetainingCapacity();
        it.gen.numbers.clearRetainingCapacity();
        it.gen.strings.clearRetainingCapacity();

        const index = it.stmts[it.node_i];
        try it.gen.generateFromStmt(index);

        const instructions = try gpa.dupe(Inst, it.gen.instructions.items);
        errdefer gpa.free(instructions);

        const variables = try gpa.dupe(u8, it.gen.variables.items);
        errdefer gpa.free(variables);

        const numbers = try gpa.dupe(usize, it.gen.numbers.items);
        errdefer gpa.free(numbers);

        const strings = try gpa.dupe([]const u8, it.gen.strings.items);
        errdefer gpa.free(strings);

        it.node_i += 1;
        return .{
            .line = it.gen.line_number,
            .instructions = instructions,
            .src = &.{},
            .variables = variables,
            .numbers = numbers,
            .strings = strings,
        };
    }
};

pub fn clone(c: Chunk, gpa: Allocator) Allocator.Error!Chunk {
    const instructions = try gpa.dupe(Inst, c.instructions);
    errdefer gpa.free(instructions);
    const variables = try gpa.dupe(u8, c.variables);
    errdefer gpa.free(variables);
    const numbers = try gpa.dupe(usize, c.numbers);
    errdefer gpa.free(numbers);
    const strings = try gpa.dupe([]const u8, c.strings);
    errdefer gpa.free(strings);
    const src = try gpa.dupe(u8, c.src);
    errdefer gpa.free(src);

    return .{
        .line = c.line,
        .instructions = instructions,
        .variables = variables,
        .numbers = numbers,
        .strings = strings,
        .src = src,
    };
}

pub fn generate(gpa: Allocator, tree: *const Ast) Allocator.Error!Chunk {
    var chunkgen: ChunkGen = .{
        .gpa = gpa,
        .tree = tree,
        .line_number = undefined,
        .instructions = .empty,
        .variables = .empty,
        .numbers = .empty,
        .strings = .empty,
    };
    defer chunkgen.instructions.deinit(gpa);
    defer chunkgen.variables.deinit(gpa);
    defer chunkgen.numbers.deinit(gpa);
    defer chunkgen.strings.deinit(gpa);

    try chunkgen.generate();

    const instructions = try chunkgen.instructions.toOwnedSlice(gpa);
    errdefer gpa.free(instructions);

    const variables = try chunkgen.variables.toOwnedSlice(gpa);
    errdefer gpa.free(variables);

    const numbers = try chunkgen.numbers.toOwnedSlice(gpa);
    errdefer gpa.free(numbers);

    const strings = try chunkgen.strings.toOwnedSlice(gpa);
    errdefer gpa.free(strings);

    return .{
        .src = &.{},
        .line = chunkgen.line_number,
        .instructions = instructions,
        .variables = variables,
        .numbers = numbers,
        .strings = strings,
    };
}

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Chunk = @This();
const ChunkGen = @import("ChunkGen.zig");
const Ast = @import("Ast.zig");
