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

pub fn main(init: std.process.Init) !void {
    var iter = try init.minimal.args.iterateAllocator(init.gpa);
    defer iter.deinit();

    _ = iter.next();

    var diag: clap.Diagnostic = .{};
    var res = clap.parseEx(clap.Help, &main_params, main_parsers, &iter, .{
        .diagnostic = &diag,
        .allocator = init.gpa,
        .terminating_positional = 0,
    }) catch |err| {
        try diag.reportToFile(init.io, .stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.helpToFile(init.io, .stdout(), clap.Help, &main_params, .{});
        return;
    }

    const command = res.positionals[0] orelse return error.MissingCommand;
    try switch (command) {
        .run => runMain(init.io, init.gpa, &iter),
        .build => buildMain(init.io, init.gpa, &iter),
    };
}

fn runMain(io: Io, gpa: Allocator, iter: *std.process.Args.Iterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help Display this help and quit
        \\<str>      File to run.
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        try diag.reportToFile(io, .stderr(), err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.helpToFile(io, .stdout(), clap.Help, &params, .{});
        return;
    }

    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    if (res.positionals[0]) |source_path| {
        const source_file = try Io.Dir.cwd().openFile(io, source_path, .{});

        var source_file_reader = source_file.reader(io, &.{});
        var allocating = Io.Writer.Allocating.init(gpa);
        _ = try source_file_reader.interface.streamRemaining(&allocating.writer);
        defer allocating.deinit();

        const source = try allocating.toOwnedSlice();
        defer gpa.free(source);

        var parse_diag: fe.Diagnostic = .{};
        const options: fe.ParseOptions = .{
            .allocator = arena,
            .diag = &parse_diag,
        };

        const tokens = fe.scan(source, options) catch |err| {
            try parse_diag.reportToFile(io, .stderr(), err);
            return err;
        };
        const parsed = fe.parse(tokens, options) catch |err| {
            try parse_diag.reportToFile(io, .stderr(), err);
            return err;
        };

        var interp: Interpreter = .init(io, gpa);
        defer interp.deinit();

        try interp.execSource(parsed);
    } else {
        try REPL(io, gpa);
    }
}

fn REPL(io: Io, gpa: Allocator) !void {
    var arena_allocator: std.heap.ArenaAllocator = .init(gpa);
    defer arena_allocator.deinit();
    const arena = arena_allocator.allocator();

    var it: Interpreter = .init(io, gpa);
    defer it.deinit();

    const f_stdin: Io.File = .stdin();
    var rbuf: [1024]u8 = undefined;
    var r_stdin = f_stdin.reader(io, &rbuf);
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
            try diag.reportToFile(io, .stderr(), err);
            continue;
        };
        const stmts = fe.parse(tokens, options) catch |err| {
            try diag.reportToFile(io, .stderr(), err);
            continue;
        };

        assert(stmts.len == 1);

        try it.executeStatement(stmts[0]);
    }
}

fn buildMain(io: Io, gpa: Allocator, iter: *std.process.Args.Iterator) !void {
    _ = io;
    _ = gpa;
    _ = iter;

    return error.NotImplemented;
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const Io = std.Io;

const clap = @import("clap");

const ast = @import("ast.zig");
const fe = @import("frontend.zig");

const Interpreter = @import("Interpreter.zig");
