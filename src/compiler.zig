//! src/compiler.zig
//! I think the namespacing is wrong here.
//! Should call it like parser or something...

const std = @import("std");
const assert = std.debug.assert;
const Allocator = std.mem.Allocator;

const ast = @import("ast.zig");
const TokenTag = ast.TokenTag;
const Token = ast.Token;

pub const SyntaxError = error{
    UnterminatedString,
    UnexpectedCharacter,
    UnknownIdentifier,
    UnexpectedToken,
    NumberExceedsMaxValue,
};

pub const CompileError = SyntaxError || Allocator.Error;

/// Diagnostic infromation for the scanner (This should probably be split for both scanner and parser to avoid weird bugs)
pub const Diagnostic = struct {
    line: ?usize = null,
    expected: ?TokenTag = null,

    pub fn reportToFile(diag: Diagnostic, file: std.fs.File, err: anyerror) !void {
        var buf: [1024]u8 = undefined;
        var writer = file.writer(&buf);
        try diag.report(&writer.interface, err);
        return writer.interface.flush();
    }

    /// Report err
    pub fn report(diag: Diagnostic, stream: *std.Io.Writer, err: anyerror) !void {
        try stream.print("err: {any}\n", .{err});

        const line = diag.line orelse return;
        try stream.print("{d}\n", .{line});

        const expected = diag.expected orelse return;
        try stream.print("{s}\n", .{@tagName(expected)});

        switch (err) {
            SyntaxError.UnterminatedString => try stream.print(
                "unterminated string on line: {d}\n",
                .{line},
            ),
            SyntaxError.UnexpectedCharacter => try stream.print(
                "unexpected character on line: {d}\n",
                .{line},
            ),
            SyntaxError.UnknownIdentifier => try stream.print(
                "unexpected identifier on line: {d}\n",
                .{line},
            ),
            SyntaxError.UnexpectedToken => try stream.print(
                "unexpected token on line: {d}\n",
                .{line},
            ),
            else => try stream.print("error while scanning: {s}\n", .{@errorName(err)}),
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

    /// Helper to quickly produce a token
    fn token(self: Scanner, tag: TokenTag) Token {
        return Token{
            .line = self.line,
            .tag = tag,
            .literal = null,
        };
    }

    /// Retrieves the next token. returns null when reached the end
    pub fn scanNextToken(self: *Scanner, diagnostic: ?*Diagnostic) ?(SyntaxError!Token) {
        if (self.done) return null;

        while (self.peekAndAdvance()) |c| {
            self.start = self.current - 1;
            switch (c) {
                // Ignore whitespace
                ' ', '\t', '\r' => {},
                // Except for newlines.
                '\n' => {
                    defer self.line += 1;
                    return self.token(.eol);
                },
                // Single character tokens (with no ambiguity)
                '(' => return self.token(.left_paren),
                ')' => return self.token(.right_paren),
                ',' => return self.token(.comma),
                '-' => return self.token(.minus),
                '+' => return self.token(.plus),
                '/' => return self.token(.slash),
                '*' => return self.token(.star),
                '=' => return self.token(.equal),
                // One or two letter characters with potential ambiguity
                '>' => return self.token(if (self.match('=')) .greater_equal else .greater),
                '<' => {
                    if (self.match('=')) return self.token(.less_equal);
                    if (self.match('>')) return self.token(.less_greater);
                    return self.token(.less);
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
        return self.token(.eof);
    }

    /// Internal fn to scan for an identifier - any alphabetic token - eg keywords and vars
    fn scanIdent(self: *Scanner, diagnostic: ?*Diagnostic) SyntaxError!Token {
        while (!self.isAtEnd()) {
            switch (self.peekAssumeInBounds()) {
                'A'...'Z' => _ = self.advance(),
                else => break,
            }
        }

        const ident = self.src[self.start..self.current];

        if (ident.len == 1) {
            // Must be a var
            return Token{ .line = self.line, .tag = .@"var", .literal = .{ .@"var" = ident[0] } };
        } else if (KEYWORDS.get(ident)) |tag| {
            // Must be a keyword
            return self.token(tag);
        }

        // Just some random A...Z characters that aren't any keywords
        if (diagnostic) |diag| diag.line = self.line;
        return error.UnknownIdentifier;
    }

    /// Internal fn to scan for strings
    fn scanString(self: *Scanner, diagnostic: ?*Diagnostic) SyntaxError!Token {
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
        return Token{
            .line = self.line,
            .tag = .string,
            .literal = .{ .string = string_literal },
        };
    }

    /// Internal fn to scan for numbers
    fn scanNumber(self: *Scanner, _: ?*Diagnostic) SyntaxError!Token {
        while (!self.isAtEnd()) {
            switch (self.peekAssumeInBounds()) {
                '0'...'9' => self.advance(),
                else => break,
            }
        }

        const number_literal_str = self.src[self.start..self.current];
        const number_literal = std.fmt.parseUnsigned(u32, number_literal_str, 10) catch |err| switch (err) {
            error.InvalidCharacter => unreachable,
            error.Overflow => return error.NumberExceedsMaxValue,
        };

        return Token{
            .line = self.line,
            .tag = .number,
            .literal = .{ .number = number_literal },
        };
    }
};

/// Mapping of keywords to their TokenTag
const KEYWORDS = std.StaticStringMap(TokenTag).initComptime(.{
    .{ "PRINT", .print },
    .{ "IF", .@"if" },
    .{ "THEN", .then },
    .{ "GOTO", .goto },
    .{ "INPUT", .input },
    .{ "LET", .let },
    .{ "GOSUB", .gosub },
    .{ "RETURN", .@"return" },
    .{ "CLEAR", .clear },
    .{ "LIST", .list },
    .{ "RUN", .run },
    .{ "END", .end },
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
    src: []const Token,

    /// The currrent token being considered
    current: usize = 0,

    ///
    fn isIndexAtEnd(self: Parser, index: usize) bool {
        return index >= self.src.len;
    }

    ///
    fn isAtEnd(self: Parser) bool {
        return self.isIndexAtEnd(self.current);
    }

    ///
    fn peek(self: Parser) Token {
        assert(!self.isAtEnd());
        return self.src[self.current];
    }

    ///
    fn previous(self: Parser) Token {
        assert(self.current > 0 and self.current <= self.src.len);
        return self.src[self.current - 1];
    }

    ///
    fn peekAndAdvance(self: *Parser) ?Token {
        if (self.isAtEnd()) return null;
        defer self.current += 1;
        return self.peek();
    }

    ///
    fn advance(self: *Parser) void {
        self.current += 1;
    }

    fn consume(self: *Parser, tag: TokenTag, maybe_diag: ?*Diagnostic) CompileError!void {
        if (self.matchTag(tag)) return;

        if (maybe_diag) |diag| {
            // Note: self.previous() not safe here.
            diag.line = if (self.isAtEnd()) null else self.previous().line;
            diag.expected = tag;
        }
        return error.UnexpectedToken;
    }

    fn matchTag(self: *Parser, tag: TokenTag) bool {
        if (self.isAtEnd()) return false;
        if (self.peek().tag != tag) return false;
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
        const current_tag = self.peek().tag;

        // This should be converted to a switch by the optimizer but not sure
        inline for (tag_args_info.@"struct".fields) |field| {
            // assert(field.type)

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
            break :blk self.previous();
        } else null;

        const root: ast.Root = .{
            .line = line_number,
            .stmt = try self.parseStmt(opts),
        };

        if (self.match(.{ .eol, .eof })) return root;

        // Line did not end with EOL or EOF
        if (opts.diag) |diag| diag.line = self.previous().line;
        return error.UnexpectedToken;
    }

    fn parseStmt(self: *Parser, opts: ParseOptions) CompileError!ast.Stmt {
        if (self.isAtEnd()) {
            if (opts.diag) |diag| diag.line = self.previous().line;
            return error.UnexpectedToken;
        }

        // Note this whole thing is a code smell
        switch (self.peekAndAdvance().?.tag) {
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
                if (opts.diag) |diag| diag.line = self.previous().line;
                return error.UnexpectedToken;
            },
        }
    }

    fn parseIfStmt(self: *Parser, opts: ParseOptions) CompileError!ast.IfStmt {
        assert(self.previous().tag == .@"if");

        // Note: This is a BinaryExpr.
        const lhs_expr = try self.parseExpr(opts);
        const relop = if (self.match(.{
            .less_greater,
            .less_equal,
            .less,
            // .greater_less,
            .greater_equal,
            .greater,
            .equal,
        }))
            self.previous()
        else {
            if (opts.diag) |diag| diag.line = self.previous().line;
            return error.UnexpectedToken;
        };
        const rhs_expr = try self.parseExpr(opts);
        try self.consume(.then, opts.diag);
        const stmt = try self.parseStmt(opts);

        return ast.IfStmt{
            .lhs_expr = lhs_expr,
            .relop = relop,
            .rhs_expr = rhs_expr,
            .stmt = stmt,
        };
    }

    fn parseLetStmt(self: *Parser, opts: ParseOptions) CompileError!ast.LetStmt {
        assert(self.previous().tag == .let);

        try self.consume(.@"var", opts.diag);
        const @"var" = self.previous();

        try self.consume(.equal, opts.diag);

        return .{
            .@"var" = @"var",
            .expr = try self.parseExpr(opts),
        };
    }

    fn parseExprListValue(self: *Parser, opts: ParseOptions) CompileError!ast.ExprListValue {
        if (self.matchTag(.string)) {
            return ast.ExprListValue{ .string = self.previous() };
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
            .@"var" = self.previous(),
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
            return ast.Expr{ .literal = self.previous() };
        }
        return self.parseGrouping(opts);
    }

    fn parseTerm(self: *Parser, opts: ParseOptions) CompileError!ast.Expr {
        var expr = try self.parseFactor(opts);

        while (self.match(.{ .star, .slash })) {
            const binary_expr = try opts.allocator.create(ast.BinaryExpr);
            binary_expr.* = .{
                .lhs = expr,
                .op = self.previous(),
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
                .op = self.previous(),
                .rhs = try self.parseTerm(opts),
            };
            break :blk ast.Expr{ .unary = unary_expr };
        } else try self.parseTerm(opts);

        while (self.match(.{ .plus, .minus })) {
            const binary_expr = try opts.allocator.create(ast.BinaryExpr);
            binary_expr.* = .{
                .lhs = expr,
                .op = self.previous(),
                .rhs = try self.parseTerm(opts),
            };

            expr = ast.Expr{ .binary = binary_expr };
        }

        return expr;
    }
};
