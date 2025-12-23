//! src/compiler.zig
//! I think the namespacing is wrong here.
//! Should call it like parser or something...

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");

pub const SyntaxError = error{
    UnterminatedString,
    UnexpectedCharacter,
    UnknownIdentifier,
    UnexpectedToken,
};

pub const CompileError = SyntaxError || Allocator.Error || error{Overflow};

/// Diagnostic infromation for the scanner (This should probably be split for both scanner and parser to avoid weird bugs)
/// TODO: Switch out diagnostic implementation based on current mode (immediate/delayed)
pub const Diagnostic = struct {
    line: ?usize = null,

    /// Report an error to a file
    pub fn reportToFile(diag: Diagnostic, file: std.fs.File, err: anyerror) !void {
        var buf: [1024]u8 = undefined;
        var writer = file.writer(&buf);
        try diag.report(&writer.interface, err);
        return writer.interface.flush();
    }

    /// Report an error
    pub fn report(diag: Diagnostic, stream: *std.Io.Writer, err: anyerror) !void {
        if (diag.line) |line| {
            switch (err) {
                SyntaxError.UnterminatedString => try stream.print("unterminated string on line: {d}\n", .{line}),
                SyntaxError.UnexpectedCharacter => try stream.print("unexpected character on line: {d}\n", .{line}),
                SyntaxError.UnknownIdentifier => try stream.print("unexpected identifier on line: {d}\n", .{line}),
                SyntaxError.UnexpectedToken => try stream.print("unexpected token on line: {d}\n", .{line}),
                else => try stream.print("{s} on line {d}\n", .{ @errorName(err), line }),
            }
        }
    }
};

/// Scanner
pub const Scanner = struct {
    /// Tiny Basic src code
    src: []const u8,

    /// Start of current token in consideration
    start: usize = 0,

    /// index of current character in consideration
    current: usize = 0,

    /// Current line
    line: usize = 1,

    // Hack. Should use optional instead but too lazy
    done: bool = false,

    /// Whether the given index is at the end
    fn isIndexAtEnd(self: Scanner, index: usize) bool {
        return index >= self.src.len;
    }

    /// Whether finished parsing src code
    fn isAtEnd(self: Scanner) bool {
        return self.isIndexAtEnd(self.current);
    }

    /// Peeks the current value
    fn peekAssumeInBounds(self: Scanner) u8 {
        assert(!self.isAtEnd());
        return self.src[self.current];
    }

    /// Returns the current value then advances
    fn peekAndAdvance(self: *Scanner) ?u8 {
        if (self.isAtEnd()) return null;
        defer self.current += 1;
        return self.peekAssumeInBounds();
    }

    /// Advance the input
    fn advance(self: *Scanner) void {
        self.current += 1;
    }

    // Advance only if the peeked value matches 'expected'
    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.peekAssumeInBounds() != expected) return false;
        self.advance();
        return true;
    }

    /// Retrieves the next token. returns null when reached the end
    pub fn scanNextToken(self: *Scanner, diagnostic: ?*Diagnostic) ?(SyntaxError!ast.Token) {
        if (self.done) return null;

        while (self.peekAndAdvance()) |c| {
            self.start = self.current - 1;
            switch (c) {
                // Ignore whitespace
                ' ', '\t', '\r' => {},
                // Except for newlines.
                '\n' => {
                    defer self.line += 1;
                    return ast.Token{ .eol = {} };
                },
                // Single character tokens (with no ambiguity)
                '(' => return ast.Token{ .left_paren = {} },
                ')' => return ast.Token{ .right_paren = {} },
                ',' => return ast.Token{ .comma = {} },
                '-' => return ast.Token{ .minus = {} },
                '+' => return ast.Token{ .plus = {} },
                '/' => return ast.Token{ .slash = {} },
                '*' => return ast.Token{ .star = {} },
                '=' => return ast.Token{ .equal = {} },
                // One or two letter characters with potential ambiguity
                '>' => return if (self.match('=')) ast.Token{ .greater_equal = {} } else ast.Token{ .greater = {} },
                '<' => {
                    if (self.match('=')) return ast.Token{ .less_equal = {} };
                    if (self.match('>')) return ast.Token{ .less_greater = {} };
                    return ast.Token{ .less = {} };
                },
                // Identifiers
                'A'...'Z' => return try self.scanIdent(diagnostic),
                // Literals
                '"' => return try self.scanString(diagnostic),
                '0'...'9' => return try self.scanNumber(diagnostic),
                else => {
                    if (diagnostic) |diag| diag.line = self.line;
                    return error.UnexpectedCharacter;
                },
            }
        }

        // Mark as done to avoid emitting multiple EOF
        self.done = true;
        return ast.Token{ .eof = {} };
    }

    /// Internal fn to scan for an identifier - any alphabetic token - eg keywords and vars
    fn scanIdent(self: *Scanner, diagnostic: ?*Diagnostic) SyntaxError!ast.Token {
        while (!self.isAtEnd()) {
            switch (self.peekAssumeInBounds()) {
                'A'...'Z' => _ = self.advance(),
                else => break,
            }
        }

        const ident = self.src[self.start..self.current];

        if (ident.len == 1) {
            // Must be a var
            return ast.Token{ .@"var" = ident[0] };
        } else if (KEYWORDS.get(ident)) |tkn| {
            return tkn;
        }

        // Just some random A...Z characters that aren't any keywords
        if (diagnostic) |diag| diag.line = self.line;
        return error.UnknownIdentifier;
    }

    /// Internal fn to scan for strings
    fn scanString(self: *Scanner, diagnostic: ?*Diagnostic) SyntaxError!ast.Token {
        // Have already scanned opening '""'
        // Scan until closing '"'
        while (!self.isAtEnd()) {
            switch (self.peekAssumeInBounds()) {
                '\n' => {
                    if (diagnostic) |diag| diag.line = self.line;
                    return error.UnterminatedString;
                },
                '"' => break,
                else => self.advance(),
            }
        }

        // Did we reach the end or '"'?
        if (self.isAtEnd()) {
            if (diagnostic) |diag| diag.line = self.line;
            return error.UnterminatedString;
        }

        // Advance over closing '"'
        self.advance();

        const string_literal = self.src[self.start + 1 .. self.current - 1];
        return ast.Token{ .string = string_literal };
    }

    /// Internal fn to scan for numbers
    fn scanNumber(self: *Scanner, _: ?*Diagnostic) SyntaxError!ast.Token {
        while (!self.isAtEnd()) {
            switch (self.peekAssumeInBounds()) {
                '0'...'9' => self.advance(),
                else => break,
            }
        }

        const number_str = self.src[self.start..self.current];
        const number = std.fmt.parseUnsigned(u32, number_str, 10) catch |err| switch (err) {
            error.InvalidCharacter => unreachable,
            error.Overflow => @panic("TODO: Handle overflow."),
        };

        return ast.Token{
            .number = number,
        };
    }
};

/// Mapping of keywords to their TokenTag
const KEYWORDS = std.StaticStringMap(ast.Token).initComptime(.{
    .{ "PRINT", ast.Token{ .print = {} } },
    .{ "IF", ast.Token{ .@"if" = {} } },
    .{ "THEN", ast.Token{ .then = {} } },
    .{ "GOTO", ast.Token{ .goto = {} } },
    .{ "INPUT", ast.Token{ .input = {} } },
    .{ "LET", ast.Token{ .let = {} } },
    .{ "GOSUB", ast.Token{ .gosub = {} } },
    .{ "RETURN", ast.Token{ .@"return" = {} } },
    .{ "CLEAR", ast.Token{ .clear = {} } },
    .{ "LIST", ast.Token{ .list = {} } },
    .{ "RUN", ast.Token{ .run = {} } },
    .{ "END", ast.Token{ .end = {} } },
});

///
pub const ParseOptions = struct {
    /// allocator, Arena makes most sense here
    allocator: Allocator,

    /// Diagnostic
    diag: ?*Diagnostic,
};

/// Parser
pub const Parser = struct {
    /// The tokens that need to be parsed
    src: []const ast.Token,

    /// The currrent token being considered
    current: usize = 0,

    /// Count the number of lines from the start of src to the current line.
    /// Lines start at one.
    fn countLines(self: Parser) usize {
        var lines: usize = 1;
        for (self.src) |t| switch (t) {
            .eol => lines += 1,
            else => {},
        };

        return lines;
    }

    ///
    fn isIndexAtEnd(self: Parser, index: usize) bool {
        return index >= self.src.len;
    }

    ///
    fn isAtEnd(self: Parser) bool {
        return self.isIndexAtEnd(self.current);
    }

    ///
    fn peek(self: Parser) ast.Token {
        assert(!self.isAtEnd());
        return self.src[self.current];
    }

    ///
    fn previous(self: Parser) ast.Token {
        assert(self.current > 0 and self.current <= self.src.len);
        return self.src[self.current - 1];
    }

    ///
    fn peekAndAdvance(self: *Parser) ?ast.Token {
        if (self.isAtEnd()) return null;
        defer self.current += 1;
        return self.peek();
    }

    ///
    fn advance(self: *Parser) void {
        self.current += 1;
    }

    fn consume(self: *Parser, tag: ast.TokenKind, maybe_diag: ?*Diagnostic) CompileError!void {
        if (self.matchTag(tag)) return;

        if (maybe_diag) |diag| {
            // Note: self.previous() not safe here.
            diag.line = if (self.isAtEnd()) null else self.countLines();
        }
        return error.UnexpectedToken;
    }

    fn matchTag(self: *Parser, tag: ast.TokenKind) bool {
        if (self.isAtEnd()) return false;
        if (self.peek() != tag) return false;
        self.advance();
        return true;
    }

    /// Matches and advances if the tag_args are any of the current tokens tag
    fn match(self: *Parser, tag_args: anytype) bool {
        const TagArgsType = @TypeOf(tag_args);
        const tag_args_info = @typeInfo(TagArgsType);
        if (tag_args_info != .@"struct") {
            @compileError("Expected a struct");
        }

        if (self.isAtEnd()) return false;
        const current_tag = std.meta.activeTag(self.peek());

        // This should be converted to a switch by the optimizer but not sure
        inline for (tag_args_info.@"struct".fields) |field| {
            if (@field(tag_args, field.name) == current_tag) {
                self.advance();
                return true;
            }
        }

        return false;
    }

    /// Recover by advancing to next stmt candidate
    pub fn synchronize(self: *Parser) void {
        while (self.peekAndAdvance()) |token| {
            if (token.tag == .eol) break;
        }
    }

    // TODO:
    pub fn parseNextStmt(self: *Parser, opts: ParseOptions) ?(CompileError!ast.Root) {
        while (self.match(.{ .eol, .eof })) {}

        if (self.isAtEnd()) {
            return null;
        }

        return self.parseRoot(opts);
    }

    ///
    fn parseRoot(self: *Parser, opts: ParseOptions) CompileError!ast.Root {
        // Check for line number
        const line_number = if (self.matchTag(.number)) blk: {
            // Safe to unwrap since we've advanced just before and it's known to be a NUMBER
            break :blk self.previous().number;
        } else null;

        const root: ast.Root = .{
            .line = line_number,
            .stmt = try self.parseStmt(opts),
        };

        if (self.match(.{ .eol, .eof })) return root;

        // Line did not end with EOL or EOF
        if (opts.diag) |diag| diag.line = self.countLines();
        return error.UnexpectedToken;
    }

    fn parseStmt(self: *Parser, opts: ParseOptions) CompileError!ast.Stmt {
        if (self.isAtEnd()) {
            if (opts.diag) |diag| diag.line = self.countLines();
            return error.UnexpectedToken;
        }

        // Note this whole thing is a code smell
        switch (self.peekAndAdvance().?) {
            .print => return ast.Stmt{ .print = try self.parseExprList(opts) },
            .@"if" => {
                const if_stmt = try opts.allocator.create(ast.IfStmt);
                if_stmt.* = try self.parseIfStmt(opts);
                return ast.Stmt{ .@"if" = if_stmt };
            },
            .goto => return ast.Stmt{ .goto = try self.parseExpr(opts) },
            .input => return ast.Stmt{ .input = try self.parseVarList(opts) },
            .let => return ast.Stmt{ .let = try self.parseLetStmt(opts) },
            .gosub => return ast.Stmt{ .gosub = try self.parseExpr(opts) },
            .@"return" => return ast.Stmt{ .@"return" = {} },
            .clear => return ast.Stmt{ .clear = {} },
            .list => return ast.Stmt{ .list = {} },
            .run => return ast.Stmt{ .run = {} },
            .end => return ast.Stmt{ .end = {} },
            else => {
                self.current -= 1;
                if (opts.diag) |diag| diag.line = self.countLines();
                return error.UnexpectedToken;
            },
        }
    }

    fn parseIfStmt(self: *Parser, opts: ParseOptions) CompileError!ast.IfStmt {
        assert(self.previous() == .@"if");

        // Note: This is a BinaryExpr.
        const lhs_expr = try self.parseExpr(opts);
        const cmp = if (self.match(.{
            .less_greater,
            .less_equal,
            .less,
            .greater_less,
            .greater_equal,
            .greater,
            .equal,
        }))
            self.previous()
        else {
            if (opts.diag) |diag| diag.line = self.countLines();
            return error.UnexpectedToken;
        };
        const rhs_expr = try self.parseExpr(opts);
        try self.consume(.then, opts.diag);
        const stmt = try self.parseStmt(opts);

        return ast.IfStmt{
            .lhs_expr = lhs_expr,
            .relop = cmp.relop().?,
            .rhs_expr = rhs_expr,
            .stmt = stmt,
        };
    }

    fn parseLetStmt(self: *Parser, opts: ParseOptions) CompileError!ast.LetStmt {
        assert(self.previous() == .let);

        try self.consume(.@"var", opts.diag);
        const token = self.previous();

        try self.consume(.equal, opts.diag);

        return .{
            .@"var" = token.@"var",
            .expr = try self.parseExpr(opts),
        };
    }

    fn parseExprListValue(self: *Parser, opts: ParseOptions) CompileError!ast.ExprListValue {
        if (self.matchTag(.string)) {
            return ast.ExprListValue{ .string = self.previous().string };
        }
        return ast.ExprListValue{ .expr = try self.parseExpr(opts) };
    }

    fn parseExprList(self: *Parser, opts: ParseOptions) CompileError!ast.ExprList {
        var head = ast.ExprList{
            .expr = try self.parseExprListValue(opts),
            .next = null,
        };

        var current = &head;
        while (self.matchTag(.comma)) {
            const new = try opts.allocator.create(ast.ExprList);
            new.* = .{
                .expr = try self.parseExprListValue(opts),
                .next = null,
            };

            current.next = new;
            current = new;
        }

        return head;
    }

    fn parseVarList(self: *Parser, opts: ParseOptions) CompileError!ast.VarList {
        try self.consume(.@"var", opts.diag);

        var head = ast.VarList{
            .@"var" = self.previous().@"var",
            .next = null,
        };

        // Fancy recursive matching
        if (self.matchTag(.comma)) {
            const new = try opts.allocator.create(ast.VarList);
            new.* = try self.parseVarList(opts);

            head.next = new;
        }

        return head;
    }

    fn parseGrouping(self: *Parser, opts: ParseOptions) CompileError!ast.Expr {
        try self.consume(.left_paren, opts.diag);
        const inner_expr = try opts.allocator.create(ast.Expr);
        inner_expr.* = try self.parseExpr(opts);
        try self.consume(.right_paren, opts.diag);
        return .{ .grouping = inner_expr };
    }

    fn parseFactor(self: *Parser, opts: ParseOptions) CompileError!ast.Expr {
        if (self.match(.{ .@"var", .number })) {
            return ast.Expr{ .literal = self.previous().valued_literal().? };
        }
        return self.parseGrouping(opts);
    }

    fn parseTerm(self: *Parser, opts: ParseOptions) CompileError!ast.Expr {
        var expr = try self.parseFactor(opts);

        while (self.match(.{ .star, .slash })) {
            const binary_expr = try opts.allocator.create(ast.BinaryExpr);
            binary_expr.* = .{
                .lhs = expr,
                .op = self.previous().op().?,
                .rhs = try self.parseFactor(opts),
            };
            expr = .{ .binary = binary_expr };
        }

        return expr;
    }

    fn parseExpr(self: *Parser, opts: ParseOptions) CompileError!ast.Expr {
        var expr = if (self.match(.{ .plus, .minus })) blk: {
            const unary_expr = try opts.allocator.create(ast.UnaryExpr);
            unary_expr.* = .{
                .op = self.previous().op().?,
                .rhs = try self.parseTerm(opts),
            };
            break :blk ast.Expr{ .unary = unary_expr };
        } else try self.parseTerm(opts);

        while (self.match(.{ .plus, .minus })) {
            const binary_expr = try opts.allocator.create(ast.BinaryExpr);
            binary_expr.* = .{
                .lhs = expr,
                .op = self.previous().op().?,
                .rhs = try self.parseTerm(opts),
            };

            expr = ast.Expr{ .binary = binary_expr };
        }

        return expr;
    }
};

pub fn scan(src: []const u8, options: ParseOptions) ![]ast.Token {
    var tokens: std.ArrayList(ast.Token) = .empty;
    defer tokens.deinit(options.allocator);

    var scanner: Scanner = .{ .src = src };
    while (scanner.scanNextToken(null)) |token_or_err| {
        const token = try token_or_err;
        try tokens.append(options.allocator, token);
    }
    return tokens.toOwnedSlice(options.allocator);
}

pub fn parse(src: []const ast.Token, options: ParseOptions) ![]ast.Root {
    var stmt_list: std.ArrayList(ast.Root) = .empty;
    defer stmt_list.deinit(options.allocator);

    var parser: Parser = .{ .src = src };
    while (parser.parseNextStmt(options)) |stmt_or_err| {
        try stmt_list.append(options.allocator, try stmt_or_err);
    }

    return try stmt_list.toOwnedSlice(options.allocator);
}
