//! src/Interpreter.zig

pub const Interpreter = @This();

/// For convenience
gpa: Allocator,

/// The index of the current line.
curr: usize = 0,

/// All of the instructions
instructions: std.ArrayList(Instruction) = .empty,

/// Stack containing lines
/// push on gosub
/// pop on return
return_stack: std.ArrayList(usize) = .empty,

// Variables.
variables: [math.maxInt(u8)]isize,

/// Whether or not the instruction should advance
/// This should only ever false on a GOTO and GOSUB
should_advance: bool = false,

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

fn indexOfLine(self: Interpreter, line: usize) usize {
    return std.sort.upperBound(Instruction, self.instructions.items, line, struct {
        pub fn compareFn(ctx: usize, instruction: Instruction) math.Order {
            return math.order(ctx, instruction.line);
        }
    }.compareFn);
}

fn indexOfLineStrict(self: Interpreter, line: usize) ?usize {
    return std.sort.binarySearch(Instruction, self.instructions.items, line, struct {
        pub fn compareFn(ctx: usize, instruction: Instruction) math.Order {
            return math.order(ctx, instruction.line);
        }
    }.compareFn);
}

pub fn executeStatement(self: *Interpreter, root: ast.Root) Allocator.Error!void {
    self.should_advance = true;

    if (root.line == null)
        return self.executeImmediately(root.stmt);

    const instruction_index = self.indexOfLine(root.line.?.literal.?.number);
    try self.instructions.insert(self.gpa, instruction_index, .{
        .line = root.line.?.literal.?.number,
        .stmt = root.stmt,
    });
}

fn executeImmediately(self: *Interpreter, stmt: ast.Stmt) void {
    self.eval(stmt);
    self.run();
}

fn run(self: *Interpreter) void {
    while (self.curr < self.instructions.items.len) {
        self.should_advance = true;
        self.eval(self.instructions.items[self.curr].stmt);
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

    if (self.should_advance)
        self.curr += 1;
}

fn evalPrint(self: *Interpreter, list: ast.ExprList) void {
    switch (list.expr) {
        .string => |str| std.debug.print("{s}", .{str.literal.?.string}),
        .expr => |expr| std.debug.print("{d}", .{self.computeExpr(expr)}),
    }

    if (list.next) |next|
        self.evalPrintCont(next.*)
    else
        std.debug.print("\n", .{});
}

fn evalPrintCont(self: Interpreter, list: ast.ExprList) void {
    switch (list.expr) {
        .string => |str| std.debug.print(" {s}", .{str.literal.?.string}),
        .expr => |expr| std.debug.print(" {d}", .{self.computeExpr(expr)}),
    }

    if (list.next) |next|
        self.evalPrintCont(next.*)
    else
        std.debug.print("\n", .{});
}

fn evalIfStmt(self: *Interpreter, if_stmt: ast.IfStmt) void {
    const lhs = self.computeExpr(if_stmt.lhs_expr);
    const rhs = self.computeExpr(if_stmt.rhs_expr);

    const then = switch (if_stmt.relop.tag) {
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
    self.should_advance = false;
}

fn evalInput(self: *Interpreter, list: ast.VarList) void {
    _ = self;
    _ = list;
    @panic("TODO");
}

fn evalLet(self: *Interpreter, stmt: ast.LetStmt) void {
    self.variables[stmt.@"var".literal.?.@"var"] = self.computeExpr(stmt.expr);
}

fn evalGosub(self: *Interpreter, expr: ast.Expr) void {
    const line = self.computeExpr(expr);
    const index = self.indexOfLineStrict(@intCast(line)) orelse @panic("TODO");
    // TODO:
    // If the very first instruction is an immediately evaluated GOTO
    // Then the return_stack will be broken.
    self.return_stack.append(self.gpa, self.instructions.items[self.curr].line) catch @panic("TODO");
    self.curr = index;

    self.should_advance = false;
}

fn evalReturn(self: *Interpreter) void {
    const line = self.return_stack.pop() orelse @panic("TODO");
    const index = self.indexOfLineStrict(line).?;
    self.curr = index;
}

fn evalClear(self: *Interpreter) void {
    _ = self;
    @panic("TODO");
}

fn evalList(self: *Interpreter) void {
    _ = self;
    @panic("TODO");
}

fn evalRun(self: *Interpreter) void {
    _ = self;
    @panic("TODO");
}

fn evalEnd(self: *Interpreter) void {
    self.should_advance = false;
    self.curr = math.maxInt(usize);
}

fn computeExpr(self: Interpreter, expr: ast.Expr) isize {
    return switch (expr) {
        .literal => |token| switch (token.literal.?) {
            .number => token.literal.?.number,
            .@"var" => self.variables[token.literal.?.@"var"],
            .string => unreachable,
        },
        .unary => |unary| switch (unary.op.tag) {
            .minus => -1 * self.computeExpr(unary.rhs),
            else => unreachable,
        },
        .binary => |binary| switch (binary.op.tag) {
            .minus => self.computeExpr(binary.lhs) - self.computeExpr(binary.rhs),
            .plus => self.computeExpr(binary.lhs) + self.computeExpr(binary.rhs),
            .slash => @panic("TODO"),
            .star => self.computeExpr(binary.lhs) * self.computeExpr(binary.rhs),
            else => unreachable,
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
