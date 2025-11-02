const std = @import("std");
const Buffer = std.ArrayList;

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
	const program = parse(&mem, contents) catch |err| {
		std.debug.print("Error in parse stream: {}\n", .{err});
		return;
	};
	show_program(program);
}

const Part = union(enum) {
	literal: []u8,
	ref: struct {
		name: []u8,
		judge: ?[]u8
	}
};

const Bind = struct {
	left: Buffer(Part),
	subbinds: ?*Program,
	right: Buffer(Buffer(Part)),

	pub fn init(mem: *const std.mem.Allocator) Bind {
		return Bind {
			.left = Buffer(Part).init(mem.*),
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

pub fn parse(mem: *const std.mem.Allocator, text: []u8) ParseError!Program {
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
		const left = try parse_left(mem, &i, text);
		const bind = try parse_right(mem, &i, text, left);
		program.append(bind)
			catch unreachable;
	}
	return program;
}

pub fn parse_right(mem: *const std.mem.Allocator, i: *u64, text: []u8, left: Buffer(Part)) ParseError!Bind {
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
			if (current_bind.right.items.len != 0){
				std.debug.print("Expected alternate for equation right hand side, found =\n", .{});
				return ParseError.UnexpectedToken;
			}
			if (current_alt.items.len == 0){
				std.debug.print("Expected right hand side for equation, found =\n", .{});
				return ParseError.UnexpectedToken;
			}
			i.* += 1;
			var nested_bind = Bind.init(mem);
			nested_bind.left = current_alt;
			if (current_bind.subbinds == null){
				current_bind.subbinds = mem.create(Buffer(Bind))
					catch unreachable;
				current_bind.subbinds.?.* = Buffer(Bind).init(mem.*);
			}
			current_bind.subbinds.?.append(try parse_right(mem, i, text, current_alt))
				catch unreachable;
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
		if (c == '"'){
			var end = i.* + 1;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c=='"'){
					break;
				}
			}
			const part = Part{
				.literal=text[i.*+1..end]
			};
			i.* = end;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		var end = i.* + 1;
		while (end < text.len) : (end += 1){
			c = text[end];
			if (c == ' ' or c == '\n' or c == '\t' or c == '|' or c == ';' or c == '=' or c == ':'){
				break;
			}
		}
		var part = Part{
			.ref=.{
				.name=text[i.*..end],
				.judge=null
			}
		};
		c = text[end];
		if (c == ':'){
			end += 1;
			i.* = end;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c == ' ' or c == '\n' or c == '\t' or c == '|' or c == ';' or c == '='){
					break;
				}
			}
			part.ref.judge = text[i.*..end];
		}
		i.* = end-1;
		current_alt.append(part)
			catch unreachable;
	}
	return ParseError.UnexpectedEOF;
}

pub fn parse_left(mem: *const std.mem.Allocator, i: *u64, text: []u8) ParseError!Buffer(Part) {
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
		if (c == '='){
			if (current_alt.items.len == 0){
				return ParseError.UnexpectedToken;
			}
			i.* += 1;
			return current_alt;
		}
		if (c == '"'){
			var end = i.* + 1;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c=='"'){
					break;
				}
			}
			const part = Part{
				.literal=text[i.*+1..end]
			};
			i.* = end;
			current_alt.append(part)
				catch unreachable;
			continue;
		}
		var end = i.* + 1;
		while (end < text.len) : (end += 1){
			c = text[end];
			if (c == ' ' or c == '\n' or c == '\t' or c == '=' or c == ':'){
				break;
			}
		}
		var part = Part{
			.ref=.{
				.name=text[i.*..end],
				.judge=null
			}
		};
		c = text[end];
		if (c == ':'){
			end += 1;
			i.* = end;
			while (end < text.len) : (end += 1){
				c = text[end];
				if (c == ' ' or c == '\n' or c == '\t' or c == '='){
					break;
				}
			}
			part.ref.judge = text[i.*..end];
		}
		i.* = end-1;
		current_alt.append(part)
			catch unreachable;
	}
	return ParseError.UnexpectedEOF;
}

pub fn show_program(program: Buffer(Bind)) void {
	for (program.items) |bind| {
		show_bind(bind);
	}
}

pub fn show_bind(bind: Bind) void {
	for (bind.left.items) |p| {
		show_part(p);
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
			std.debug.print("{s} ", .{part.literal});
		},
		.ref => {
			std.debug.print("{s} ", .{part.ref.name});
			if (part.ref.judge) |judge| {
				std.debug.print(": {s} ", .{judge});
			}
		}
	}
}
