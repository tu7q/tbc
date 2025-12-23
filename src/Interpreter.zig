//! src/Interpreter.zig

pub const Interpreter = @This();

/// For convenience
gpa: Allocator,

/// The index of the current line.
curr: ?usize = null,

/// All of the instructions
instructions: std.ArrayList(Instruction) = .empty,

/// Stack lines to goto
/// Push via gosub
/// Pop via return
return_stack: std.ArrayList(usize) = .empty,

// Variables.
variables: [math.maxInt(u8)]isize,

advance_with_step: bool = false,

const Instruction = struct {
    stmt: ast.Stmt,
    line: usize,
};

pub fn init(gpa: std.mem.Allocator) Interpreter {
    return .{
        .gpa = gpa,
        .variables = [_]isize{0} ** math.maxInt(u8),
    };
}

pub fn deinit(self: *Interpreter) void {
    self.instructions.deinit(self.gpa);
    self.return_stack.deinit(self.gpa);
    self.* = undefined;
}

pub fn execSource(self: *Interpreter, src: []const ast.Root) !void {
    for (src) |stmt| {
        try self.executeStatement(stmt);
    }
    if (self.instructions.items.len > 0) {
        self.curr = 0;
        self.run();
    }
}

fn indexOfLine(self: Interpreter, line: usize) usize {
    const upper = std.sort.upperBound(Instruction, self.instructions.items, line, struct {
        pub fn compareFn(ctx: usize, instruction: Instruction) math.Order {
            return math.order(ctx, instruction.line);
        }
    }.compareFn);
    if (upper > 0 and self.instructions.items[upper - 1].line == line) {
        return upper - 1;
    }
    return upper;
}

fn indexOfLineStrict(self: Interpreter, line: usize) ?usize {
    return std.sort.binarySearch(Instruction, self.instructions.items, line, struct {
        pub fn compareFn(ctx: usize, instruction: Instruction) math.Order {
            return math.order(ctx, instruction.line);
        }
    }.compareFn);
}

pub fn executeStatement(self: *Interpreter, root: ast.Root) Allocator.Error!void {
    if (root.line) |line| {
        const index = self.indexOfLine(line);
        if (index < self.instructions.items.len and self.instructions.items[index].line == line) {
            self.instructions.items[index].stmt = root.stmt;
        } else {
            try self.instructions.insert(self.gpa, index, .{
                .line = line,
                .stmt = root.stmt,
            });
        }
    } else {
        assert(self.curr == null);
        return self.executeImmediately(root.stmt);
    }
}

fn executeImmediately(self: *Interpreter, stmt: ast.Stmt) void {
    self.eval(stmt);
    self.run();
}

fn run(self: *Interpreter) void {
    while (self.curr) |curr| {
        self.advance_with_step = true;
        self.eval(self.instructions.items[curr].stmt);

        if (self.curr != null) if (self.advance_with_step) {
            self.curr.? += 1;
            if (self.curr.? >= self.instructions.items.len) {
                self.curr = null;
            }
        };
    }
}

fn eval(self: *Interpreter, stmt: ast.Stmt) void {
    switch (stmt) {
        .print => |list| self.evalPrint(list),
        .@"if" => |if_stmt| self.evalIfStmt(if_stmt.*),
        .goto => |expr| self.evalGoto(expr),
        .input => |list| self.evalInput(list),
        .let => |let_stmt| self.evalLet(let_stmt),
        .gosub => |expr| self.evalGosub(expr),
        .@"return" => self.evalReturn(),
        .clear => self.evalClear(),
        .list => self.evalList(),
        .run => self.evalRun(),
        .end => self.evalEnd(),
    }
}

fn evalPrint(self: *Interpreter, list: ast.ExprList) void {
    switch (list.expr) {
        .string => |str| std.debug.print("{s}", .{str}),
        .expr => |expr| std.debug.print("{d}", .{self.computeExpr(expr)}),
    }

    if (list.next) |next| {
        std.debug.print(" ", .{});
        self.evalPrint(next.*);
    } else std.debug.print("\n", .{});
}

fn evalIfStmt(self: *Interpreter, if_stmt: ast.IfStmt) void {
    const lhs = self.computeExpr(if_stmt.lhs_expr);
    const rhs = self.computeExpr(if_stmt.rhs_expr);

    const then = switch (if_stmt.relop) {
        .equal => lhs == rhs,
        .greater => lhs > rhs,
        .greater_equal => lhs >= rhs,
        .less => lhs < rhs,
        .less_equal => lhs <= rhs,
        .less_greater => lhs != rhs,
        else => unreachable,
    };

    if (then)
        self.eval(if_stmt.stmt);
}

fn evalGoto(self: *Interpreter, expr: ast.Expr) void {
    const line = self.computeExpr(expr);
    const index = self.indexOfLineStrict(@intCast(line)) orelse @panic("TODO");
    self.curr = index;
    self.advance_with_step = false;
}

fn evalInput(self: *Interpreter, list: ast.VarList) void {
    const stdin = std.fs.File.stdin();
    var buf: [1024]u8 = undefined;
    var stdin_reader = stdin.reader(&buf);
    const reader = &stdin_reader.interface;

    // Hidden allocation yay...
    var line_alloc: std.Io.Writer.Allocating = .init(self.gpa);
    defer line_alloc.deinit();

    _ = reader.streamDelimiter(&line_alloc.writer, '\n') catch @panic("");
    _ = reader.takeByte() catch @panic("");

    var it = std.mem.splitScalar(u8, line_alloc.written(), ' ');
    var node: ?*const ast.VarList = &list;
    while (node != null) : (node = node.?.next) {
        const str = it.next() orelse @panic("Bad input");

        const value: isize = std.fmt.parseInt(isize, str, 10) catch str[0];
        self.variables[node.?.@"var"] = value;
    }
}

fn evalLet(self: *Interpreter, stmt: ast.LetStmt) void {
    self.variables[stmt.@"var"] = self.computeExpr(stmt.expr);
}

fn evalGosub(self: *Interpreter, expr: ast.Expr) void {
    const line = self.computeExpr(expr);
    const index = self.indexOfLineStrict(@intCast(line)) orelse @panic("TODO");
    // TODO:
    // If the very first instruction is an immediately evaluated GOTO
    // Then the return_stack will be broken.
    self.return_stack.append(self.gpa, self.instructions.items[self.curr.?].line) catch @panic("TODO");
    self.curr = index;
    self.advance_with_step = false;
}

fn evalReturn(self: *Interpreter) void {
    const line = self.return_stack.pop() orelse @panic("TODO");
    const index = self.indexOfLineStrict(line).?;
    self.curr = index;
}

fn evalClear(_: *Interpreter) void {
    std.debug.print("\x1B[2J\x1B[H", .{});
}

fn evalList(self: *Interpreter) void {
    const stdout = std.fs.File.stdout();
    var stdout_writer = stdout.writer(&.{});

    for (self.instructions.items) |instruction| {
        fmt.prettyPrint(
            .{ .line = instruction.line, .stmt = instruction.stmt },
            &stdout_writer.interface,
        ) catch @panic("");
    }
}

fn evalRun(self: *Interpreter) void {
    self.curr = if (self.instructions.items.len > 0) 0 else null;
}

fn evalEnd(self: *Interpreter) void {
    self.curr = null;
}

fn computeExpr(self: Interpreter, expr: ast.Expr) isize {
    return switch (expr) {
        .literal => |token| switch (token) {
            .number => |n| @intCast(n),
            .@"var" => |v| self.variables[v],
            .string => unreachable,
        },
        .unary => |unary| switch (unary.op) {
            .minus => -1 * self.computeExpr(unary.rhs),
            else => unreachable,
        },
        .binary => |binary| switch (binary.op) {
            .minus => self.computeExpr(binary.lhs) - self.computeExpr(binary.rhs),
            .plus => self.computeExpr(binary.lhs) + self.computeExpr(binary.rhs),
            .div => @panic("TODO"),
            .mul => self.computeExpr(binary.lhs) * self.computeExpr(binary.rhs),
        },
        .grouping => |grouping| self.computeExpr(grouping.*),
    };
}

const ast = @import("ast.zig");
const fmt = @import("fmt.zig");

const std = @import("std");
const Allocator = std.mem.Allocator;

const math = std.math;

const assert = std.debug.assert;
