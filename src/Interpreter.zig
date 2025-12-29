//! src/Interpreter.zig

/// Allocator.
gpa: Allocator,

/// All chunks stored by the interpreter
/// sorted by line number.
chunks: std.MultiArrayList(Chunk),

/// All variables of the interpreter
variables: [256]isize,

/// The value stack.
value_stack: std.ArrayListUnmanaged(isize),

/// Return stack.
return_stack: std.ArrayListUnmanaged(Chunk.LineNumber),

/// index of the chunk being executed
chunk_i: usize,

/// index of the instruction being executed
inst_i: usize,

/// index of the next variable.
var_i: usize,

/// index of the next number.
num_i: usize,

str_i: usize,

cmp_flag: bool,

/// Interpreter input
r: *std.Io.Reader,

/// Interpreter output.
w: *std.Io.Writer,

buf: []const u8,
buf_i: usize,

p_delimiter: []const u8 = ", ",
p_flush: []const u8 = "\n",

pub const Error = error{
    DivideByZero,
    InvalidJumpAddress,
    EmptyValueStack,
    EmptyReturnStack,
    LoadOutOfBounds,
} || Allocator.Error;

pub fn init(gpa: Allocator, r: *std.Io.Reader, w: *std.Io.Writer) Allocator.Error!Interpreter {
    var chunks: std.MultiArrayList(Chunk) = .empty;
    errdefer chunks.deinit(gpa);
    try chunks.append(gpa, .{
        .line = .immediate,
        .instructions = &.{},
        .src = &.{},
        .variables = &.{},
        .numbers = &.{},
        .strings = &.{},
    });
    return .{
        .gpa = gpa,
        .chunks = chunks,
        .variables = [_]isize{0} ** 256,
        .value_stack = .empty,
        .return_stack = .empty,
        .chunk_i = undefined,
        .inst_i = undefined,
        .var_i = undefined,
        .num_i = undefined,
        .str_i = undefined,
        .cmp_flag = undefined,
        .r = r,
        .w = w,
        .buf = &.{},
        .buf_i = 0,
    };
}

pub fn deinit(int: *Interpreter) void {
    const slice = int.chunks.slice();
    for (0..slice.len) |i| {
        var c = slice.get(i);
        c.deinit(int.gpa);
    }
    int.chunks.deinit(int.gpa);
    int.value_stack.deinit(int.gpa);
    int.return_stack.deinit(int.gpa);
    int.* = undefined;
}

const IndexTag = enum {
    variable,
    number,
    string,
    inst,
};

fn resetChunkIndices(int: *Interpreter, chunk: usize) void {
    int.chunk_i = chunk;
    int.var_i = 0;
    int.num_i = 0;
    int.str_i = 0;
    int.inst_i = 0;
    int.buf_i = 0;
}

fn nextIndex(int: *Interpreter, tag: IndexTag) usize {
    const index = switch (tag) {
        .variable => &int.var_i,
        .number => &int.num_i,
        .string => &int.str_i,
        .inst => &int.inst_i,
    };
    const result = index.*;
    index.* += 1;
    return result;
}

fn chunkVariable(int: *Interpreter) Error!u8 {
    assert(int.chunk_i < int.chunks.len);
    const index = int.nextIndex(.variable);
    const variables = int.chunks.items(.variables)[int.chunk_i];
    if (index >= variables.len) return error.LoadOutOfBounds;
    return variables[index];
}

fn chunkNumber(int: *Interpreter) Error!usize {
    assert(int.chunk_i < int.chunks.len);
    const index = int.nextIndex(.number);
    const numbers = int.chunks.items(.numbers)[int.chunk_i];
    if (index >= numbers.len) return error.LoadOutOfBounds;
    return numbers[index];
}

fn chunkString(int: *Interpreter) Error![]const u8 {
    assert(int.chunk_i < int.chunks.len);
    const index = int.nextIndex(.string);
    const strings = int.chunks.items(.strings)[int.chunk_i];
    if (index >= strings.len) return error.LoadOutOfBounds;
    return strings[index];
}

fn chunkInst(int: *Interpreter) ?Chunk.Inst {
    assert(int.chunk_i < int.chunks.len);
    const index = int.nextIndex(.inst);
    const instructions = int.chunks.items(.instructions)[int.chunk_i];
    if (index >= instructions.len) return null;
    return instructions[index];
}

fn pop_value(int: *Interpreter) Error!isize {
    return int.value_stack.pop() orelse return error.EmptyValueStack;
}

fn push_value(int: *Interpreter, value: isize) Error!void {
    try int.value_stack.append(int.gpa, value);
}

fn pop_return(int: *Interpreter) Error!Chunk.LineNumber {
    return int.return_stack.pop() orelse return error.EmptyReturnStack;
}

fn push_return(int: *Interpreter, line: Chunk.LineNumber) Error!void {
    try int.return_stack.append(int.gpa, line);
}

/// Reset and run until program exits.
pub fn run(int: *Interpreter) Error!void {
    if (1 < int.chunks.len) {
        int.resetChunkIndices(1);
        try int.exec();
    }
}

pub fn handleChunk(int: *Interpreter, chunk: Chunk) Error!void {
    try int.chunks.ensureUnusedCapacity(int.gpa, 1);
    const chunk_clone = try chunk.clone(int.gpa);

    const index = std.sort.equalRange(Chunk.LineNumber, int.chunks.items(.line), chunk.line, struct {
        pub fn compareFn(ctx: Chunk.LineNumber, elem: Chunk.LineNumber) std.math.Order {
            return std.math.order(@intFromEnum(ctx), @intFromEnum(elem));
        }
    }.compareFn).@"0";

    if (index < int.chunks.len) {
        if (int.chunks.items(.line)[index] == chunk.line) {
            var c = int.chunks.get(index);
            c.deinit(int.gpa);
            int.chunks.set(index, chunk_clone);
        } else {
            int.chunks.insertAssumeCapacity(index, chunk_clone);
        }
    } else {
        int.chunks.appendAssumeCapacity(chunk_clone);
    }

    if (chunk.line == .immediate) {
        int.resetChunkIndices(0);
        try int.exec();
    }
}

fn jmpIndex(int: *Interpreter, line: usize) Error!usize {
    return std.sort.binarySearch(Chunk.LineNumber, int.chunks.items(.line), line, struct {
        pub fn cmp(ctx: usize, i: Chunk.LineNumber) std.math.Order {
            return std.math.order(ctx, @intFromEnum(i));
        }
    }.cmp) orelse return error.InvalidJumpAddress;
}

fn fetchInst(int: *Interpreter) ?Chunk.Inst {
    if (int.chunk_i >= int.chunks.len) return null;
    // if (int.chunk_i == 0 and
    //     int.chunks.items(.line)[int.chunk_i] == .immediate)
    // {
    //     return null;
    // }
    return int.chunkInst() orelse {
        int.resetChunkIndices(int.chunk_i + 1);
        return int.fetchInst();
    };
}

fn exec(int: *Interpreter) Error!void {
    while (true) {
        const maybe_inst =
            if (int.chunk_i == 0)
                int.chunkInst()
            else
                int.fetchInst();
        const inst = maybe_inst orelse return;
        // std.debug.print("inst_i: {d}, chunk_i: {d}, inst: {any}\n", .{ int.inst_i - 1, int.chunk_i, inst });
        switch (inst) {
            .cmp_lt,
            .cmp_lte,
            .cmp_gt,
            .cmp_gte,
            .cmp_eq,
            .cmp_ne,
            => {
                const b = try int.pop_value(); // pop rhs
                const a = try int.pop_value(); // pop lhs
                int.cmp_flag = switch (inst) {
                    .cmp_lt => a < b,
                    .cmp_lte => a <= b,
                    .cmp_gt => a > b,
                    .cmp_gte => a >= b,
                    .cmp_eq => a == b,
                    .cmp_ne => a != b,
                    else => unreachable,
                };
            },
            .load_number => {
                const number = try int.chunkNumber();
                try int.push_value(@intCast(number));
            },
            .load_variable => {
                const variable = try int.chunkVariable();
                try int.push_value(int.variables[variable]);
            },
            .add,
            .sub,
            .mul,
            .div,
            => {
                const b = try int.pop_value(); // pop rhs
                const a = try int.pop_value(); // pop lhs
                try int.push_value(switch (inst) {
                    .add => a + b,
                    .sub => a - b,
                    .mul => a * b,
                    .div => if (b == 0) return error.DivideByZero else @divTrunc(a, b),
                    else => unreachable,
                });
            },
            .negate => {
                try int.push_value(-try int.pop_value());
            },
            .print_num => {
                int.w.print("{d}", .{try int.pop_value()}) catch @panic("TODO");
            },
            .print_str => {
                int.w.print("{s}", .{try int.chunkString()}) catch @panic("TODO");
            },
            .print_delimiter => {
                int.w.writeAll(int.p_delimiter) catch @panic("TODO");
            },
            .print_flush => {
                int.w.writeAll(int.p_flush) catch @panic("TODO");
            },
            .break_if_not_cmp => {
                if (!int.cmp_flag) {
                    int.resetChunkIndices(int.chunk_i + 1);
                }
            },
            .goto => {
                const iline = try int.pop_value();
                if (iline <= 0) return error.InvalidJumpAddress;
                const index = try int.jmpIndex(@intCast(iline));
                int.resetChunkIndices(index);
            },
            .gosub => {
                const return_line = int.chunks.items(.line)[int.chunk_i];
                if (return_line == .immediate) return error.InvalidJumpAddress;
                const jmp_line = try int.pop_value();
                if (jmp_line <= 0) return error.InvalidJumpAddress;

                try int.push_return(return_line);
                errdefer _ = int.pop_return() catch unreachable;

                const index = try int.jmpIndex(@intCast(jmp_line));
                int.resetChunkIndices(index);
            },
            .read_line => {
                int.gpa.free(int.buf);
                var allocating: std.Io.Writer.Allocating = .init(int.gpa);
                defer allocating.deinit();

                _ = int.r.streamDelimiter(&allocating.writer, '\n') catch @panic("TODO");
                _ = int.r.takeByte() catch @panic("TODO");
                int.buf = try allocating.toOwnedSlice();
                int.buf_i = 0;
            },
            .push_val => {
                var it = std.mem.tokenizeAny(u8, int.buf, &std.ascii.whitespace);

                // TODO: Properly store tokenizer state.
                for (0..int.buf_i) |_| {
                    _ = it.next();
                }

                const v = try int.chunkVariable();

                if (it.next()) |s| {
                    int.variables[v] = std.fmt.parseInt(isize, s, 10) catch |err| switch (err) {
                        error.InvalidCharacter => s[0],
                        error.Overflow => @panic("TODO"),
                    };
                } else {
                    int.variables[v] = 0;
                }

                int.buf_i += 1;
            },
            .assign => {
                const a = try int.pop_value();
                const v = try int.chunkVariable();
                int.variables[v] = a;
            },
            .@"return" => {
                const line = try int.pop_return();
                // We inserted a line at an index. That line should
                // still exist.
                const index = int.jmpIndex(@intFromEnum(line)) catch unreachable;
                int.resetChunkIndices(index + 1);
            },
            .clear => {
                int.w.writeAll("\x1B[2J\x1B[H") catch @panic("TODO");
            },
            .list => @panic("NOT IMPLEMENTED"),
            .run => int.resetChunkIndices(1),
            .end => int.resetChunkIndices(std.math.maxInt(usize)),
        }
    }
}

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;
const Interpreter = @This();
const Chunk = @import("Chunk.zig");
