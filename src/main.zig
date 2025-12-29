//! src/main.zig

// Global stdout,sterr,stidn because I am lazy.
var stdout: *Io.Writer = undefined;
var stderr: *Io.Writer = undefined;
var stdin: *Io.Reader = undefined;

const SubCommands = enum {
    run,
    fmt,
    pbc,
};

const main_parsers = .{
    .command = clap.parsers.enumeration(SubCommands),
};

const main_params = clap.parseParamsComptime(
    \\-h,--help Display this help and exit
    \\<command>
);

pub fn main() !void {
    var stderr_writer = fs.File.writer(.stderr(), &.{});
    stderr = &stderr_writer.interface;
    defer _ = stderr.flush() catch {};

    var stdout_writer = fs.File.writer(.stdout(), &.{});
    stdout = &stdout_writer.interface;
    defer _ = stdout.flush() catch {};

    var buf: [1024]u8 = undefined;
    var stdin_reader = fs.File.reader(.stdin(), &buf);
    stdin = &stdin_reader.interface;

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
        try diag.report(stderr, err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(stdout, clap.Help, &main_params, .{});
        return;
    }

    const command = res.positionals[0] orelse {
        try stderr.writeAll(
            \\missing command, expected one of "run", "fmt", "pbc".
        );
        return;
    };
    try switch (command) {
        .run => runMain(gpa, &iter),
        .fmt => fmtMain(gpa, &iter),
        .pbc => pbcMain(gpa, &iter),
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

    if (res.positionals[0]) |source_path| {
        const source_file = try fs.cwd().openFile(source_path, .{});

        var allocating = Io.Writer.Allocating.init(gpa);
        defer allocating.deinit();

        var buf: [1024]u8 = undefined;
        var file_reader = source_file.reader(&buf);
        _ = try (&file_reader.interface).streamRemaining(&allocating.writer);

        const source: [:0]const u8 = try allocating.toOwnedSliceSentinel(0);
        defer gpa.free(source);

        var ast: Ast = try .parse(gpa, source, .all);
        defer ast.deinit(gpa);

        if (ast.errors.len > 0) {
            for (ast.errors) |err| {
                std.debug.print("{any}\n", .{err});
            }
            return error.Bad;
        }

        var interpreter: Interpreter = try .init(gpa, stdin, stdout);
        defer interpreter.deinit();

        var it: Chunk.Iterator = .init(gpa, &ast);
        defer it.deinit();
        while (true) {
            var chunk = try it.next(gpa) orelse break;
            defer chunk.deinit(gpa);

            try interpreter.handleChunk(chunk);
        }

        try interpreter.run();
    } else {
        return REPL(gpa);
    }
}

fn REPL(gpa: Allocator) !void {
    var aline = std.Io.Writer.Allocating.init(gpa);
    defer aline.deinit();

    var interpreter: Interpreter = try .init(gpa, stdin, stdout);
    defer interpreter.deinit();

    while (true) {
        _ = try stdin.streamDelimiter(&aline.writer, '\n');
        _ = try stdin.takeByte(); // Skip '\n'
        const line = try aline.toOwnedSliceSentinel(0);

        var tree: Ast = try .parse(gpa, line, .chunked);
        defer tree.deinit(gpa);

        if (tree.errors.len > 0) {
            for (tree.errors) |err| {
                try stderr.print("{any}\n", .{err});
            }
            continue;
        }

        var chunk: Chunk = try .generate(gpa, &tree);
        defer chunk.deinit(gpa);

        try interpreter.handleChunk(chunk);
    }
}

fn fmtMain(gpa: Allocator, iter: *std.process.ArgIterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help Display this help and quit
        \\<str>      File to format.
    );

    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        try diag.report(stderr, err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(stdout, clap.Help, &params, .{});
        return;
    }

    const file_path = res.positionals[0] orelse return error.MissingFilePath;

    const source = blk: {
        const file = try std.fs.cwd().openFile(file_path, .{});
        defer file.close();

        break :blk try allocSourceFile(gpa, file);
    };
    defer gpa.free(source);

    var tree: Ast = try .parse(gpa, source, .all);
    defer tree.deinit(gpa);

    if (tree.errors.len > 0) {
        std.log.err("cannot format file with errors", .{});
        return;
    }

    const file = try std.fs.cwd().createFile(file_path, .{});
    defer file.close();

    try Fmt.printTreeToFile(file, tree);
}

fn pbcMain(gpa: Allocator, iter: *std.process.ArgIterator) !void {
    const params = comptime clap.parseParamsComptime(
        \\-h, --help  Display this help and quit
        \\<str>       Source file.
    );
    var diag = clap.Diagnostic{};
    var res = clap.parseEx(clap.Help, &params, clap.parsers.default, iter, .{
        .diagnostic = &diag,
        .allocator = gpa,
    }) catch |err| {
        try diag.report(stderr, err);
        return err;
    };
    defer res.deinit();

    if (res.args.help != 0) {
        try clap.help(stdout, clap.Help, &params, .{});
        return;
    }

    const file_path = res.positionals[0] orelse return error.MissingFilePath;
    const file = try fs.cwd().openFile(file_path, .{});
    defer file.close();
    const src = try allocSourceFile(gpa, file);
    defer gpa.free(src);
    var tree: Ast = try .parse(gpa, src, .all);
    defer tree.deinit(gpa);
    var it: Chunk.Iterator = .init(gpa, &tree);
    defer it.deinit();
    while (true) {
        var chunk = (try it.next(gpa)) orelse break;
        defer chunk.deinit(gpa);
        try stdout.print("line: {d}, src: {s}\n", .{
            @intFromEnum(chunk.line),
            chunk.src,
        });

        var v_i: usize = 0;
        var n_i: usize = 0;
        var s_i: usize = 0;

        for (chunk.instructions) |inst| {
            try stdout.print("\t{any}", .{inst});
            switch (inst) {
                .load_number => {
                    try stdout.print("({d})", .{chunk.numbers[n_i]});
                    n_i += 1;
                },
                .load_variable => {
                    try stdout.print("({c})", .{chunk.variables[v_i]});
                    v_i += 1;
                },
                .print_str => {
                    try stdout.print("({s})\n", .{chunk.strings[s_i]});
                    s_i += 1;
                },
                .push_val => {
                    try stdout.print("({c})", .{chunk.variables[v_i]});
                    v_i += 1;
                },
                .assign => {
                    try stdout.print("({c})", .{chunk.variables[v_i]});
                    v_i += 1;
                },
                else => {},
            }
            try stdout.print("\n", .{});
        }
    }
}

fn allocSourceFile(gpa: Allocator, file: std.fs.File) ![:0]const u8 {
    var buf: [1024]u8 = undefined;
    var freader = file.reader(&buf);
    const reader = &freader.interface;

    var allocating: std.Io.Writer.Allocating = try .initCapacity(
        gpa,
        file.getEndPos() catch 1,
    );
    const w = &allocating.writer;

    _ = try reader.streamRemaining(w);

    return allocating.toOwnedSliceSentinel(0);
}

const std = @import("std");
const Allocator = std.mem.Allocator;
const assert = std.debug.assert;
const fs = std.fs;
const Io = std.Io;

const clap = @import("clap");

const Ast = @import("Ast.zig");
const Fmt = @import("Fmt.zig");
const ChunkGen = @import("ChunkGen.zig");
const Chunk = @import("Chunk.zig");
const Interpreter = @import("Interpreter.zig");
