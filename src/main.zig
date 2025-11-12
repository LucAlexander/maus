const std = @import("std");
const Buffer = std.ArrayList;

const Error = struct {
	message: []u8,
	pos: u64
};

pub fn main() !void {
	const allocator = std.heap.page_allocator;
	var main_mem = std.heap.ArenaAllocator.init(allocator);
	defer main_mem.deinit();
	const mem = main_mem.allocator();
	var infile = std.fs.cwd().openFile("plugin.maus", .{}) catch {
		std.debug.print("File not found: {s}\n", .{"plugin.maus"});
		return;
	};
	defer infile.close();
	const stat = infile.stat() catch {
		std.debug.print("Errored file stat: {s}\n", .{"plugin.maus"});
		return;
	};
	const contents = infile.readToEndAlloc(allocator, stat.size+1) catch {
		std.debug.print("Error reading file: {s}\n", .{"plugin.maus"});
		return;
	};
	defer allocator.free(contents);
	var error_log = Buffer(Error).init(mem);
	const tokens = tokenize(&mem, contents, &error_log);
	if (error_log.items.len != 0){
		for (error_log.items) |err| {
			show_error(contents, err);
		}
		return;
	}
	const program = parse_program(&mem, tokens.items, &error_log);
	if (error_log.items.len != 0){
		for (error_log.items) |err| {
			show_error(contents, err);
		}
		return;
	}
	show_program(program);
	var vast = VAST.init(&mem, program, &error_log);
	vast.run(&error_log);
}

const TOKEN = enum {
	ID,
	OR,
	EQ,
	OPEN,
	CLOSE,
	SEMI,
	UNBIND,
	LIT	
};

const Token = struct {
	pos: u64,
	text: []u8,
	tag: TOKEN
};

pub fn tokenize(mem: *const std.mem.Allocator, text: []u8, err: *Buffer(Error)) Buffer(Token) {
	var i: u64 = 0;
	var tokens = Buffer(Token).init(mem.*);
	outer: while (i < text.len) {
		var c = text[i];
		while (c == ' ' or c == '\t' or c == '\n') {
			i += 1;
			if (i == text.len){
				return tokens;
			}
			c = text[i];
		}
		switch (c){
			'=' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i..i+1],
					.tag = .EQ
				}) catch unreachable;
				i += 1;
				continue;
			},
			'|' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i..i+1],
					.tag = .OR
				}) catch unreachable;
				i += 1;
				continue;
			},
			'(' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i..i+1],
					.tag = .OPEN
				}) catch unreachable;
				i += 1;
				continue;
			},
			')' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i..i+1],
					.tag = .CLOSE
				}) catch unreachable;
				i += 1;
				continue;
			},
			';' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i..i+1],
					.tag = .SEMI
				}) catch unreachable;
				i += 1;
				continue;
			},
			'/' => {
				tokens.append(Token {
					.pos = i,
					.text = text[i .. i+1],
					.tag = .UNBIND
				}) catch unreachable;
				i += 1;
				continue;
			},
			else => { }
		}
		if (c == '"'){
			i += 1;
			const start = i;
			while (i < text.len){
				if (text[i] == '"'){
					tokens.append(Token {
						.pos = start,
						.text = text[start .. i],
						.tag = .LIT
					}) catch unreachable;
					i += 1;
					continue :outer;
				}
				i += 1;
			}
			err.append(set_error(mem, i, "Unexpected end of file in literal token\n", .{}))
				catch unreachable;
			return tokens;
		}
		if (std.ascii.isAlphanumeric(c) or c == '_'){
			const start = i;
			while (std.ascii.isAlphanumeric(c) or c == '_') {
				i += 1;
				if (i == text.len){
					err.append(set_error(mem, i, "Unexpected end of file in token\n", .{}))
						catch unreachable;
					return tokens;
				}
				c = text[i];
			}
			tokens.append(Token {
				.pos = start,
				.text = text[start .. i],
				.tag = .ID
			}) catch unreachable;
			continue;
		}
		err.append(set_error(mem, i, "Unexpected symbol in token stream {c}", .{text[i]}))
			catch unreachable;
		return tokens;
	}
	return tokens;
}

pub fn set_error(mem: *const std.mem.Allocator, index:u64, comptime fmt: []const u8, args: anytype) Error {
	var err = Error{
		.pos = index,
		.message = mem.alloc(u8, 128) catch unreachable,
	};
	const result = std.fmt.bufPrint(err.message, fmt, args)
		catch unreachable;
	err.message.len = result.len;
	return err;
}

pub fn show_error(text: []u8, err: Error) void {
	var i: u64 = 0;
	var dist:u64 = 32;
	if (err.pos < dist){
		dist = err.pos;
	}
	var start_pos:u64 = 0;
	var end_pos:u64 = text.len;
	var found_start = false;
	var line:u64 = 1;
	var start_line:u64 = 1;
	while (i < text.len){
		if (text[i] == '\n'){
			line += 1;
		}
		if (i>err.pos-dist and !found_start){
			if (text[i] == '\n'){
				start_pos = i+1;
				found_start = true;
				start_line = line;
			}
		}
		if (i > err.pos+dist){
			if (text[i] == '\n'){
				end_pos = i;
				break;
			}
		}
		i += 1;
	}
	line = start_line;
	const stderr = std.io.getStdErr().writer();
	stderr.print("\x1b[1m{s}\x1b[0m\n", .{err.message})
		catch unreachable;
	stderr.print("{d:06} | ", .{line})
		catch unreachable;
	for (start_pos .. end_pos) |k| {
		if (text[k] == '\n'){
			line += 1;
			stderr.print("\n{d:06} | ", .{line})
				catch unreachable;
			continue;
		}
		if (k == err.pos){
			stderr.print("\x1b[1;4;31m", .{})
				catch unreachable;
		}
		std.debug.print("{c}", .{text[k]});
		if (k == err.pos){
			stderr.print("\x1b[0m", .{})
				catch unreachable;
		}
	}
	stderr.print("\n", .{})
		catch unreachable;
}

const ParseError = error {
	UnexpectedToken,
	UnexpectedEOF
};

const Program = Buffer(Equation);

const Equation = union(enum){
	bind: struct {
		left: Side,
		rules: Program,
		right: Side
	},
	unbind: Side
};

const Side = Buffer(Alt);

const Alt = struct {
	name: Token,
	args: Buffer(Arg)
};

const Arg = union(enum){
	named: Equation,
	literal: Token,
	unnamed: Side,
	simple: Token
};

pub fn parse_program(mem: *const std.mem.Allocator, tokens: []Token, err: *Buffer(Error)) Program {
	var i: u64 = 0;
	var program = Buffer(Equation).init(mem.*);
	while (i < tokens.len){
		const t = tokens[i];
		if (t.tag == .UNBIND){
			program.append(parse_unbind(mem, &i, tokens, err, .SEMI) catch {
				continue;
			}) catch unreachable;
		}
		else {
			program.append(parse_bind(mem, &i, tokens, err, .SEMI) catch {
				continue;
			}) catch unreachable;
		}
	}
	return program;
}

pub fn parse_subprogram(mem: *const std.mem.Allocator, i: *u64, tokens: []Token, err: *Buffer(Error)) ParseError!Program {
	var program = Buffer(Equation).init(mem.*);
	while (i.* < tokens.len){
		const t = tokens[i.*];
		if (t.tag == .UNBIND){
			const save = i.*;
			program.append(parse_unbind(mem, i, tokens, err, .SEMI) catch {
				i.* = save;
				return program;
			}) catch unreachable;
		}
		else {
			const save = i.*;
			program.append(parse_bind(mem, i, tokens, err, .SEMI) catch {
				i.* = save;
				return program;
			}) catch unreachable;
		}
	}
	err.append(set_error(mem, tokens[i.*-1].pos, "Unepected end of file in nested program parse\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn parse_unbind(mem: *const std.mem.Allocator, i: *u64, tokens: []Token, err: *Buffer(Error), end_token: TOKEN) ParseError!Equation {
	var t = tokens[i.*];
	std.debug.assert(t.tag == .UNBIND);
	i.* += 1;
	t = tokens[i.*];
	return Equation {
		.unbind = try parse_side(mem, i, tokens, err, end_token)
	};
}

pub fn parse_bind(mem: *const std.mem.Allocator, i: *u64, tokens: []Token, err: *Buffer(Error), end_token: TOKEN) ParseError!Equation {
	const left = try parse_side(mem, i, tokens, err, .EQ);
	const save = i.*;
	const right = parse_side(mem, i, tokens, err, .SEMI) catch {
		i.* = save;
		const sub = try parse_subprogram(mem, i, tokens, err);
		const right = try parse_side(mem, i, tokens, err, end_token);
		return Equation {
			.bind = .{
				.left = left,
				.rules = sub,
				.right = right
			}
		};
	};
	return Equation {
		.bind = .{
			.left = left,
			.rules = Buffer(Equation).init(mem.*),
			.right = right
		}
	};
}

pub fn parse_side(mem: *const std.mem.Allocator, i: *u64, tokens: []Token, err: *Buffer(Error), end_token: TOKEN) ParseError!Side {
	var side = Buffer(Alt).init(mem.*);
	while (i.* < tokens.len){
		const alt = try parse_alt(mem, i, tokens, err, end_token);
		side.append(alt)
			catch unreachable;
		const t = tokens[i.*];
		if (t.tag == .OR){
			i.* += 1;
			continue;
		}
		std.debug.assert(t.tag == end_token);
		i.* += 1;
		return side;
	}
	err.append(set_error(mem, tokens[i.*-1].pos, "Unexpected End of File in side parse\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn parse_alt(mem: *const std.mem.Allocator, i: *u64, tokens: []Token, err: *Buffer(Error), end_token: TOKEN) ParseError!Alt {
	const name = tokens[i.*];
	if (name.tag != .ID) {
		err.append(set_error(mem, tokens[i.*].pos, "Expected identifier for alternate name, found {s}\n", .{name.text}))
			catch unreachable;
		return ParseError.UnexpectedToken;
	}
	i.* += 1;
	var args = Buffer(Arg).init(mem.*);
	while (i.* < tokens.len){
		const t = tokens[i.*];
		if (t.tag == .OPEN){
			const save = i.*;
			const bind = parse_bind(mem, i, tokens, err, .CLOSE) catch {
				i.* = save;
				const side = try parse_side(mem, i, tokens, err, .CLOSE);
				args.append(Arg {
					.unnamed = side
				}) catch unreachable;
				continue;
			};
			args.append(Arg {
				.named = bind
			}) catch unreachable;
			continue;
		}
		const argname = tokens[i.*];
		if (argname.tag == .LIT){
			args.append(Arg {
				.literal = argname
			}) catch unreachable;
			i.* += 1;
			continue;
		}
		if (argname.tag == .OR or argname.tag == end_token){
			return Alt {
				.name=name,
				.args=args
			};
		}
		if (argname.tag != .ID){
			err.append(set_error(mem, tokens[i.*].pos, "Expected identifier for argument type, found {s}\n", .{argname.text}))
				catch unreachable;
			return ParseError.UnexpectedToken;
		}
		args.append(Arg {
			.simple = argname
		}) catch unreachable;
		i.* += 1;
	}
	err.append(set_error(mem, tokens[i.*-1].pos, "Unexpected End of File encountered in alt parse\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedToken;
}

pub fn show_program(program: Buffer(Equation)) void {
	for (program.items) |eq| {
		switch (eq){
			.bind => {
				show_side(eq.bind.left);
				std.debug.print("= ", .{});
				show_program(eq.bind.rules);
				show_side(eq.bind.right);
				std.debug.print(";\n", .{});
			},
			.unbind => {
				std.debug.print("/", .{});
				show_side(eq.unbind);
			}
		}
	}
}

pub fn show_side(side: Buffer(Alt)) void {
	for (side.items, 0..) |alt, i| {
		if (i != 0){
			std.debug.print("| ", .{});
		}
		show_alt(alt);
	}
}

pub fn show_alt(alt: Alt) void {
	std.debug.print("{s} ", .{alt.name.text});
	for (alt.args.items) |arg| {
		show_arg(arg);
	}
}

pub fn show_arg(arg: Arg) void {
	switch (arg){
		.named => {
			if (arg.named == .bind){
				std.debug.print("(", .{});
				show_side(arg.named.bind.left);
				std.debug.print("= ", .{});
				show_program(arg.named.bind.rules);
				show_side(arg.named.bind.right);
				std.debug.print(") ", .{});
				return;
			}
			std.debug.print("/", .{});
			show_side(arg.named.unbind);
		},
		.literal => {
			std.debug.print("\"{s}\" ", .{arg.literal.text});
		},
		.unnamed => {
			show_side(arg.unnamed);
		},
		.simple => {
			std.debug.print("{s} ", .{arg.simple.text});
		}
	}
}

const RuntimeError = error {
	UnknownPredicate
};

const VAST = struct {
	program: Buffer(Equation),
	nodes: std.StringHashMap(*Buffer(*VastNode)),
	mem: *std.mem.Allocator,
	
	pub fn init(mem: *const std.mem.Allocator, program: Buffer(Equation), err: *Buffer(Error)) VAST {
		var vast = VAST{,
			.program = program,
			.nodes = std.StringHashMap(*Buffer(*VastNode)).init(mem.*),
			.mem = mem
		};
		for (program.items) |equation| {
			vast.add(equation, err);
		}
		return vast;
	},

	pub fn add(vast: *VAST, equation: Equation, _: *Buffer(Error)) void {
		switch (equation){
			.bind => {
				outer: for (equation.bind.left.items) |alt| {
					const name = alt.name;
					if (vast.nodes.get(name.text)) |buffer| {
						for (buffer.items) |node| {
							if (compare_args(alt.args, node.alt.args)){
								continue :outer;
							}
						}
						buffer.*.append(VastNode.init(mem, alt))
							catch unreachable;
						continue :outer;
					}
					var buffer = mem.create(Buffer(*VastNode))
						catch unreachable;
					buffer.* = Buffer(*VastNode).init(mem.*);
					buffer.append(VastNode.init(mem, alt))
						catch unreachable;
					vast.nodes.put(name.text, buffer)
						catch unreachable;
				}
			},
			.unbind => { }
		}
	},

	pub fn find(vast: *VAST, alt: Alt) ?*VastNode {
		const name = alt.name;
		if (vast.nodes.get(name.text)) |buffer| {
			for (buffer.items) |node| {
				if (compare_args(alt.args, node.alt.args)){
					return node;
				}
			}
		}
		return null;
	},

	pub fn run(vast: *VAST, err: *Buffer(Error)) void {
		const dummy = Buffer(Arg).init(vast.mem.*);
		for (vast.program.items) |equation| {
			if (equation == .bind){
				for (equation.bind.left.items) |alt| {
					dummy.clearRetainingCapacity();
					vast.compute(dummy, alt, equation.bind.rules, equation.bind.right) catch {
						continue
					};
				}
			}
			else{
				for (equation.unbind.items) |alt| {
					if (vast.find(alt)) |node| {
						node.bound = false;
						node.implications.clearRetainingCapacity();
						for (node.reverse.items) |inv| {
							var i: u64 = 0;
							while (i < inv.implications.items.len){
								var self = inv.implications.items[i];
								while (self == node){
									inv.implications.swapRemove(i);
									self = inv.implications.items[i];
								}
							}
						}
						node.reverse.clearRetainingCapacity();
					}
				}
			}
		}
	},

	pub fn compute(vast: *VAST, args: Buffer(Args), alt: Alt, program: Program, right: Side, err: *Buffer(Error)) RuntimeError!void {
		if (vast.find(alt)) |node| {
			const newargs = scrape_args(vast.mem, alt);
			const save = args.len;
			args.appendSlice(newargs)
				catch unreachable;
			for (program.items) |equation| {
				for (equation.left.items) |subalt| {
					try vast.compute(args, subalt, equation.rules, equation.right);
				}
			}
			for (right.items) |right_alt| {
				try vast.link(args, node, alt, right_alt);
			}
			args.len = save;
		}
		err.append(set_error(vast.mem, alt.name.pos, "Unable to find logical node corresponding to left side of equation\n", .{}))
			catch unreachable;
		return RuntimeError.UnknownPredicate;
	},

	pub fn link(vast: *VAST, args: Buffer(Arg), left_node: *VastNode, left: Alt, right: Alt) RuntimeError!void {
		const right_name = right.name;
		if (vast.nodes.get(name.text)) |buffer| {
			for (buffer.items) |node| {
				if (node.bound == false){
					continue;
				}
				if (node.alt.args.items.len != right.args.items.len){
					continue;
				}
				for (node.alt.args, right.args) |candidate, real| {
					if (compare_arg(candidate, real)){
						continue;
					}
					try vast.ensure_path(real, candidate, err);
				}
				//TODO ensure paths exist between double bound vals, and ensure no new equations are created on the right
				left_node.implications.append(node)
					catch unreachable;
			}
		}
	},

	pub fn find_arg_node(vast: *VAST, arg: Arg) ?*VastNode {
		//TODO
	},

	pub fn ensure_path(vast: *VAST, left: Arg, right: Arg, err: *Buffer(Error)) RuntimeError!void {

		//TODO
	}
};

pub fn scrape_args(mem: *const std.mem.Allocator, alt: Alt) Buffer(Arg) {
	var args = Buffer(Arg).init(mem.*);
	for (alt.args.items) |arg| {
		if (arg == .named){
			args.append(arg);
		}
	}
	return args;
}

pub fn compare_args(a: Buffer(Arg), b: Buffer(Arg)) bool {
	if (a.items.len != b.items.len){
		return false;
	}
	for (a.items, b.items) |left, right| {
		if (!compare_arg(left, right)){
			return false;
		}
	}
	return true;
}

pub fn compare_arg(a: Arg, b: Arg) bool {
	switch (a){
		.named => {
			if (b != .named){
				return false;
			}
			return compare_side(a.named.bind.left, b.named.bind.left) and compare_side(a.named.bind.right, b.named.bind.right);
		},
		.literal => {
			if (b != .literal){
				return false;
			}
			return std.mem.eql(u8, a.literal.text, b.literal.text);
		},
		.unnamed => {
			if (b != .unnamed){
				return false;
			}
			return compare_side(a.unnamed, b.unnamed);
		},
		.simple => {
			if (b != .simple){
				return false;
			}
			return std.mem.eql(u8, a.simple.text, b.simple.text);
		}
	}
	return false;
}

pub fn compare_side(a: Side, b: Side) bool {
	for (a.items, b.items) |left, right| {
		if (compare_alt(left, right) == false){
			return false;
		}
	}
	return true;
}

pub fn compare_alt(a: Alt, b: Alt) bool {
	if (std.mem.eql(u8, a.name.text, b.name.text) == false){
		return false;
	}
	return compare_args(a.args, b.args);
}

const VastNode = struct {
	implications: Buffer(*VastNode),
	reverse: Buffer(*VastNode),
	bound: bool,
	alt: Alt,

	pub fn init(mem: *const std.mem.Allocator, alt: Alt) *VastNode {
		const ptr = mem.create(VastNode) catch unreachable;
		ptr.* = VastNode{
			.implications = Buffer(*VastNode).init(mem.*),
			.reverse = Buffer(*VastNode).init(mem.*),
			.bound = true,
			.alt = alt
		};
		return ptr;
	}
};

