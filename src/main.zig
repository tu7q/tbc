//! src/main.zig

pub fn main() !void {
    var da_impl = std.heap.DebugAllocator(.{}){};
    defer _ = da_impl.deinit();
    const gpa = da_impl.allocator();

    const stdin = std.fs.File.stdin();
    var buf: [1024]u8 = undefined;
    var stdin_reader = stdin.reader(&buf);
    const reader = &stdin_reader.interface;

    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();

    var interpreter: Interpreter = .init(gpa);
    defer interpreter.deinit();

    while (true) {
        var line_alloc: std.Io.Writer.Allocating = .init(gpa);
        defer line_alloc.deinit();

        _ = try reader.streamDelimiterEnding(&line_alloc.writer, '\n');
        const line = line_alloc.written();

        _ = reader.takeByte() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return err,
        };
        try line_alloc.writer.writeAll("\n");

        var scanner: compiler.Scanner = .{ .src = line };
        var parse_diag: compiler.Diagnostic = .{};

        var tokens: std.ArrayListUnmanaged(ast.Token) = .empty;
        defer tokens.deinit(gpa);

        var was_err: bool = false;

        while (scanner.scanNextToken(&parse_diag)) |token_or_err| {
            const token = token_or_err catch |err| {
                try parse_diag.reportToFile(.stderr(), err);
                was_err = true;
                break;
            };

            try tokens.append(gpa, token);
        }

        if (was_err)
            continue;

        var parser: compiler.Parser = .{ .src = tokens.items };
        const options: compiler.ParseOptions = .{
            .allocator = arena.allocator(),
            .diag = &parse_diag,
        };

        while (parser.parseNextStmt(options)) |stmt_or_err| {
            const stmt = stmt_or_err catch |err| {
                try parse_diag.reportToFile(.stderr(), err);
                break;
            };

            try interpreter.executeStatement(stmt);
        }
    }
}

const std = @import("std");
const Allocator = std.mem.Allocator;

// compiler stuff
const compiler = @import("compiler.zig");
const ast = @import("ast.zig");
const fmt = @import("fmt.zig");

const Interpreter = @import("Interpreter.zig");
