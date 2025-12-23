//! src/main.zig

const SubCommands = enum {
    build,
    run,
};

const main_parsers = .{
    .command = clap.parsers.enumeration(SubCommands),
};

const main_params = clap.parseParamsComptime(
    \\-h,--help Display this help and exit
    \\<command>
);

pub fn main() !void {
    var debug_allocator = std.heap.DebugAllocator(.{}){};
    defer _ = debug_allocator.deinit();
    const gpa = debug_allocator.allocator();

    var iter = try std.process.ArgIterator.initWithAllocator(gpa);
    defer iter.deinit();

    _ = iter.next();

    var diag: clap.Diagnostic = .{};
    var res = clap.parseEx(clap.Help, &main_params, main_parsers, &iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
        .terminating_positional = 0,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.helpToFile(.stdout(), clap.Help, &main_params, .{});
        return;
    }

    const command = res.positionals[0] orelse return error.MissingCommand;
    try switch (command) {
        .run => runMain(gpa, &iter),
        .build => buildMain(gpa, &iter),
    };
}

fn runMain(gpa: Allocator, iter: *std.process.ArgIterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help Display this help and quit
        \\<str>      File to run.
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        try diag.reportToFile(.stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.helpToFile(.stdout(), clap.Help, &params, .{});
        return;
    }

    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    if (res.positionals[0]) |source_path| {
        const source_file = try fs.cwd().openFile(source_path, .{});
        const source = try source_file.readToEndAlloc(gpa, std.math.maxInt(usize));
        defer gpa.free(source);

        var parse_diag: fe.Diagnostic = .{};
        const options: fe.ParseOptions = .{
            .allocator = arena,
            .diag = &parse_diag,
        };
        const tokens = fe.scan(source, options) catch |err| {
            try parse_diag.reportToFile(.stderr(), err);
            return err;
        };
        const parsed = fe.parse(tokens, options) catch |err| {
            try parse_diag.reportToFile(.stderr(), err);
            return err;
        };

        var interp: Interpreter = .init(gpa);
        defer interp.deinit();

        try interp.execSource(parsed);
    } else {
        try REPL(gpa);
    }
}

fn REPL(gpa: Allocator) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var it: Interpreter = .init(gpa);
    defer it.deinit();

    const f_stdin: fs.File = .stdin();
    var rbuf: [1024]u8 = undefined;
    var r_stdin = f_stdin.reader(&rbuf);
    const stdin = &r_stdin.interface;

    var w_line: Io.Writer.Allocating = .init(gpa);
    defer w_line.deinit();

    while (true) {
        _ = try stdin.streamDelimiter(&w_line.writer, '\n');
        try stdin.streamExact(&w_line.writer, 1);

        const line = try w_line.toOwnedSlice();
        defer gpa.free(line);

        defer _ = arena_allocator.reset(.retain_capacity);
        var diag: fe.Diagnostic = .{};
        const options: fe.ParseOptions = .{
            .allocator = arena,
            .diag = &diag,
        };
        const tokens = fe.scan(line, options) catch |err| {
            try diag.reportToFile(.stderr(), err);
            continue;
        };
        const stmts = fe.parse(tokens, options) catch |err| {
            try diag.reportToFile(.stderr(), err);
            continue;
        };

        assert(stmts.len == 1);

        try it.executeStatement(stmts[0]);
    }
}

fn buildMain(gpa: Allocator, iter: *std.process.ArgIterator) !void {
    _ = gpa;
    _ = iter;

    return error.NotImplemented;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const fs = std.fs;
const Io = std.Io;

const clap = @import("clap");

const ast = @import("ast.zig");
const fe = @import("frontend.zig");

const Interpreter = @import("Interpreter.zig");
