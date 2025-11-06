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
	const program = parse(&mem, contents, &error_log);
	if (error_log.items.len != 0){
		for (error_log.items) |err| {
			show_error(contents, err);
		}
	}
	else{
		show_program(program);
	}
}

const Part = union(enum) {
	literal: struct {
		text: []u8,
		pos: u64
	},
	ref: struct {
		name: []u8,
		pos: u64,
		link: ?*Bind
	},
	compref: struct {
		ref: *Buffer(Part),
		right: ?*Buffer(Part),
		pos: u64,
		link: ?*Bind
	}
};

const Bind = struct {
	left: Buffer(Buffer(Part)),
	subbinds: ?*Program,
	right: Buffer(Buffer(Part)),

	pub fn init(mem: *const std.mem.Allocator) Bind {
		return Bind {
			.left = Buffer(Buffer(Part)).init(mem.*),
			.subbinds = null,
			.right = Buffer(Buffer(Part)).init(mem.*)
		};
	}
};

const Program = Buffer(Bind);

const ParseError = error {
	UnexpectedToken,
	UnexpectedEOF
};

pub fn parse(mem: *const std.mem.Allocator, text: []u8, err: *Buffer(Error)) Program {
	var i: u64 = 0;
	var program = Buffer(Bind).init(mem.*);
	outer: while (i<text.len) : (i += 1) {
		var c = text[i];
		while (c == ' ' or c == '\n' or c == '\t'){
			i += 1;
			if (i == text.len){
				break :outer;
			}
			c = text[i];
		}
		const left = parse_left(mem, &i, text, err) catch {
			continue;
		};
		const bind = parse_right(mem, &i, text, left, err) catch {
			continue;
		};
		program.append(bind)
			catch unreachable;
	}
	return program;
}

pub fn parse_right(mem: *const std.mem.Allocator, i: *u64, text: []u8, left: Buffer(Buffer(Part)), err: *Buffer(Error)) ParseError!Bind {
	var current_alt = Buffer(Part).init(mem.*);
	var current_bind = Bind.init(mem);
	current_bind.left = left;
	outer: while (i.* < text.len) : (i.* += 1){
		var c = text[i.*];
		while (c == ' ' or c == '\n' or c == '\t'){
			i.* += 1;
			if (i.* == text.len){
				break :outer;
			}
			c = text[i.*];
		}
		if (c == '='){
			if (current_alt.items.len == 0){
				err.append(set_error(mem, i.*, "Expected right hand side for equation, found =\n", .{}))
					catch unreachable;
				return ParseError.UnexpectedToken;
			}
			i.* += 1;
			current_bind.right.append(current_alt)
				catch unreachable;
			if (current_bind.subbinds == null){
				current_bind.subbinds = mem.create(Buffer(Bind))
					catch unreachable;
				current_bind.subbinds.?.* = Buffer(Bind).init(mem.*);
			}
			current_bind.subbinds.?.append(try parse_right(mem, i, text, current_bind.right, err))
				catch unreachable;
			current_bind.right = Buffer(Buffer(Part)).init(mem.*);
			current_alt = Buffer(Part).init(mem.*);
			continue;
		}
		if (c == ';'){
			current_bind.right.append(current_alt)
				catch unreachable;
			i.* += 1;
			return current_bind;
		}
		else if (c == '|'){
			current_bind.right.append(current_alt)
				catch unreachable;
			current_alt = Buffer(Part).init(mem.*);
			continue;
		}
		else if (c == '('){
			i.* += 1;
			const comp = try parse_comp_ref(mem, i, text, err);
			std.debug.assert(text[i.*] == ')' or text[i.*] == '=');
			if (text[i.*] == '='){
				i.* += 1;
				const right = try parse_comp_ref(mem, i, text, err);
				if (text[i.*] == '='){
					err.append(set_error(mem, i.*, "Encountered = in right of nested binding\n", .{}))
						catch unreachable;
					return ParseError.UnexpectedToken;
				}
				const part = Part{
					.compref = .{
						.ref=mem.create(Buffer(Part)) catch unreachable,
						.right = mem.create(Buffer(Part)) catch unreachable,
						.pos = i.*,
						.link=null
					}
				};
				part.compref.ref.* = comp;
				part.compref.right.?.* = right;
				current_alt.append(part)
					catch unreachable;
				continue;
			}
			const part = Part{
				.compref = .{
					.ref=mem.create(Buffer(Part)) catch unreachable,
					.right=null,
					.pos = i.*,
					.link=null
				}
			};
			part.compref.ref.* = comp;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		else if (c == '"'){
			var end = i.* + 1;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c=='"'){
					break;
				}
			}
			const part = Part{
				.literal=.{
					.pos = i.*,
					.text=text[i.*+1..end]
				}
			};
			i.* = end;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		var end = i.* + 1;
		while (end < text.len) : (end += 1){
			c = text[end];
			if (c == ' ' or c == '\n' or c == '\t' or c == '|' or c == ';' or c == '='){
				break;
			}
		}
		const part = Part{
			.ref=.{
				.name=text[i.*..end],
				.pos = i.*,
				.link=null
			}
		};
		i.* = end-1;
		current_alt.append(part)
			catch unreachable;
	}
	err.append(set_error(mem, i.*, "Encountered end of file while parsing right hand side of equation \n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn parse_comp_ref(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error)) ParseError!Buffer(Part) {
	var current_alt = Buffer(Part).init(mem.*);
	outer: while (i.* < text.len) : (i.* += 1){
		var c = text[i.*];
		while (c == ' ' or c == '\n' or c == '\t'){
			i.* += 1;
			if (i.* == text.len){
				break :outer;
			}
			c = text[i.*];
		}
		if (c == ')' or c=='='){
			if (current_alt.items.len == 0){
				err.append(set_error(mem, i.*, "Encountered empty nested expression in equation\n", .{}))
					catch unreachable;
				return ParseError.UnexpectedToken;
			}
			return current_alt;
		}
		else if (c == '('){
			i.* += 1;
			const comp = try parse_comp_ref(mem, i, text, err);
			std.debug.assert(text[i.*] == ')' or text[i.*] == '=');
			if (text[i.*] == '='){
				i.* += 1;
				const right = try parse_comp_ref(mem, i, text, err);
				if (text[i.*] == '='){
					//TODO error
					return ParseError.UnexpectedToken;
				}
				const part = Part{
					.compref = .{
						.ref=mem.create(Buffer(Part)) catch unreachable,
						.right = mem.create(Buffer(Part)) catch unreachable,
						.pos = i.*,
						.link=null
					}
				};
				part.compref.ref.* = comp;
				part.compref.right.?.* = right;
				current_alt.append(part)
					catch unreachable;
				continue;
			}
			const part = Part{
				.compref = .{
					.pos = i.*,
					.ref=mem.create(Buffer(Part)) catch unreachable,
					.right=null,
					.link=null
				}
			};
			part.compref.ref.* = comp;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		else if (c == '"'){
			var end = i.* + 1;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c=='"'){
					break;
				}
			}
			const part = Part{
				.literal=.{
					.text=text[i.*+1..end],
					.pos = i.*
				}
			};
			i.* = end;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		var end = i.* + 1;
		while (end < text.len) : (end += 1){
			c = text[end];
			if (c == ' ' or c == '\n' or c == '\t' or c == ')'){
				break;
			}
		}
		const part = Part{
			.ref=.{
				.name=text[i.*..end],
				.pos = i.*,
				.link=null
			}
		};
		i.* = end-1;
		current_alt.append(part)
			catch unreachable;
	}
	err.append(set_error(mem, i.*, "Encountered end of file while parsing right hand nested equation\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn parse_left(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error)) ParseError!Buffer(Buffer(Part)) {
	var current_alt = Buffer(Part).init(mem.*);
	var current_side = Buffer(Buffer(Part)).init(mem.*);
	outer: while (i.* < text.len) : (i.* += 1){
		var c = text[i.*];
		while (c == ' ' or c == '\n' or c == '\t'){
			i.* += 1;
			if (i.* == text.len){
				break :outer;
			}
			c = text[i.*];
		}
		if (c == '='){
			if (current_alt.items.len == 0){
				err.append(set_error(mem, i.*, "Encountered empty left hand side of equation\n", .{}))
					catch unreachable;
				return ParseError.UnexpectedToken;
			}
			i.* += 1;
			current_side.append(current_alt)
				catch unreachable;
			return current_side;
		}
		else if (c == '|'){
			current_side.append(current_alt)
				catch unreachable;
			current_alt = Buffer(Part).init(mem.*);
			continue;
		}
		else if (c == '('){
			i.* += 1;
			const comp = try parse_comp_ref(mem, i, text, err);
			std.debug.assert(text[i.*] == ')' or text[i.*] == '=');
			if (text[i.*] == '='){
				i.* += 1;
				const right = try parse_comp_ref(mem, i, text, err);
				if (text[i.*] == '='){
					err.append(set_error(mem, i.*, "Encountered = in right of nested binding\n", .{}))
						catch unreachable;
					return ParseError.UnexpectedToken;
				}
				const part = Part{
					.compref = .{
						.ref=mem.create(Buffer(Part)) catch unreachable,
						.right = mem.create(Buffer(Part)) catch unreachable,
						.pos = i.*,
						.link=null
					}
				};
				part.compref.ref.* = comp;
				part.compref.right.?.* = right;
				current_alt.append(part)
					catch unreachable;
				continue;
			}
			const part = Part{
				.compref = .{
					.pos=i.*,
					.ref=mem.create(Buffer(Part)) catch unreachable,
					.right=null,
					.link=null
				}
			};
			part.compref.ref.* = comp;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		else if (c == '"'){
			var end = i.* + 1;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c=='"'){
					break;
				}
			}
			const part = Part{
				.literal=.{
					.text=text[i.*+1..end],
					.pos = i.*
				}
			};
			i.* = end;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		var end = i.* + 1;
		while (end < text.len) : (end += 1){
			c = text[end];
			if (c == ' ' or c == '\n' or c == '\t' or c == '=' or c == ':' or c == '|'){
				break;
			}
		}
		const part = Part{
			.ref=.{
				.name=text[i.*..end],
				.pos = i.*,
				.link=null
			}
		};
		i.* = end-1;
		current_alt.append(part)
			catch unreachable;
	}
	err.append(set_error(mem, i.*, "Encountered end of file while parsing left side of equation\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn show_program(program: Buffer(Bind)) void {
	for (program.items) |bind| {
		show_bind(bind);
	}
}

pub fn show_bind(bind: Bind) void {
	for (bind.left.items, 0..) |list, i| {
		if (i != 0){
			std.debug.print("| ", .{});
		}
		for (list.items) |p| {
			show_part(p);
		}
	}
	std.debug.print("= ", .{});
	if (bind.subbinds) |subs| {
		std.debug.print("\n", .{});
		for (subs.items) |sub| {
			show_bind(sub);
		}
	}
	for (bind.right.items, 0..) |list, i| {
		if (i != 0){
			std.debug.print("| ", .{});
		}
		for (list.items) |p| {
			show_part(p);
		}
	}
	std.debug.print(";\n", .{});
}

pub fn show_part(part: Part) void {
	switch (part){
		.literal => {
			std.debug.print("\"{s}\" ", .{part.literal.text});
		},
		.ref => {
			std.debug.print("{s} ", .{part.ref.name});
		},
		.compref => {
			std.debug.print("( ", .{});
			for (part.compref.ref.items) |sub| {
				show_part(sub);
			}
			if (part.compref.right) |right| {
				std.debug.print("= ", .{});
				for (right.items) |sub| {
					show_part(sub);
				}
			}
			std.debug.print(") ", .{});
		}
	}
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
	const dist = 32;
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

const LinkError = error {
	ConstructNotFound
};

const VAST = struct {
	program: Buffer(Bind)
};
