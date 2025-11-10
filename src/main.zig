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
	var vast = VAST.init(&mem);
	add_binds(&mem, &vast, program, &error_log) catch {
		if (error_log.items.len != 0){
			for (error_log.items) |err| {
				show_error(contents, err);
			}
		}
		return;
	};
	std.debug.print("initial vast:\n", .{});
	show_vast(vast);
	//TODO add initial value to input_q from main
	var chunk = Buffer(Bind).init(mem);
	defer chunk.deinit();
	while (vast.input_q.pop()) |node| {
		chunk.clearRetainingCapacity();
		run_instantiations(&mem, &vast, node, &error_log, &chunk) catch {
			if (error_log.items.len != 0){
				for (error_log.items) |err| {
					show_error(contents, err);
				}
			}
			return;
		};
		add_binds(&mem, &vast, chunk, &error_log) catch {
			if (error_log.items.len != 0){
				for (error_log.items) |err| {
					show_error(contents, err);
				}
			}
			return;
		};
	}
}

const Name = union(enum) {
	literal: struct {
		text: []u8,
		pos:u64
	},
	name: {
		text: []u8,
		pos: u64
	}
};

const Arg = union(enum) {
	name: Name,
	arg: struct {
		name: Alt,
		side: Side
	},
	alt: Alt
};

const Alt = struct {
	name: []u8,
	args: Buffer(Arg)
};

const Side = Buffer(Alt);

const Equation = union (enum){
	bind: struct {
		left: Side,
		right: Side,
		subbinds: ?*Buffer(Equation)
	},
	unbind: Side
};

const Program = Buffer(Equation);

const ParseError = error {
	UnexpectedToken,
	UnexpectedEOF
};

pub fn parse(mem: *const std.mem.Allocator, text: []u8, err: *Buffer(Error)) Program {
	var i: u64 = 0;
	var program = Buffer(Equation).init(mem.*);
	outer: while (i<text.len) {
		var c = text[i];
		while (c == ' ' or c == '\n' or c == '\t'){
			i + = 1;
			if (t == text.len){
				break :outer;
			}
			c = text[i];
		}
		if (text[i] == '/') {
			const unbind = parse_unbind(mem, &i, text, err) catch {
				continue;
			};
			program.append(unbind)
				catch unreachable;
			continue;
		}
		const bind = parse_bind(mem, &i, text, err, ';') catch {
			continue;
		};
	}
} 

pub fn skip_whitespace(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error)) ParseError!void {
	var c = text[i.*];
	while (c == ' ' or c == '\n' or c == '\t'){
		i.* += 1;
		if (i == text.len){
			err.append(set_error(mem, i.*, "Unexpected End of File\n", .{}))
				catch unreachable;
			return ParseError.UnexpectedEOF;
		}
	}
}

pub fn parse_unbind(mem: *const std.mem.Allocator, i: *u64, text: []u8. err: *Buffer(Error)) ParseError!Equation {
	std.debug.assert(text[i.*] == '/');
	i.* += 1;
	const equation = Equation {
		.unbind = try parse_side(mem, i, text, err, ';')
	};
	return equation;
}

pub fn parse_bind(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error), end_symbol: u8) ParseError!Equation {
	const left = try parse_side(mem, i, text, err, '=');
	const right = //TODO
	const equation = Equation {
		.bind = .{
			.left = left,
			.right = right,
			.subbinds = subbinds
		}
	};
	return equation
}

pub fn parse_side(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error), end_symbol: u8) ParseError!Side {
	const side = Buffer(Alt).init(mem.*);
	while (i.* < text.len) {
		const alt = try parse_alt(mem, i, text, err, end_symbol);
		if (text[i.*] == '|'){
			i.* += 1;
			continue;
		}
		if (text[i.*] != end_symbol){
			err.append(set_error(mem, i.*, "Encountered unexpected end to alternate\n", .{}))
				catch unreachable;
			return PraseError.UnexpectedEOF:
		}
		return side;
	}
}

pub fn parse_alt(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error), end_symbol: u8) ParseError!Alt {
	try skip_whitespace(mem, i, text, err);
	const start = i.*;
	while (i.*<text.len) {
		const c = text[i.*];
		if (c == ' ' or c == '\t' or c == '\n'){
			const name = text[start .. i.*];
			var args = Buffer(Arg).init(mem.*);
			while (i.* < text.len){
				const arg = try parse_arg(mem, i, text, err);
				args.append(arg)
					catch unreachable;
				if (i.* == end_symbol or i.* == '|'){
					return Alt {
						.name = name,
						.args = args
					};
				}
			}
		}
		i.* += 1;
	}
	err.append(set_error(mem, i.*, "Encountered end of file while parseing alternate name\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
}

pub fn parse_arg(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error)) ParseError!Arg {
	try skip_whitespace(mem, i, text, err);
	var c = text[i.*];
	if (c == '('){
		const save = i.*;
		const eq = (mem, i, text, err, ')') catch {
			i.* = save;
			const nested = try parse_alt(mem, i, text, err, ')');
			return Arg {
				.alt = nested
			};
		};
		if (eq.subbinds) |_| {
			err.append(set_error(mem, i.*, "Encountered subbinds in argument binding\n", .{}))
				catch unreachable;
		}
		if (eq.left.len > 1){
			err.append(set_error(mem, i.*, "Encountered complex binding name where argname was expected\n", .{}))
				catch unreachable;
		}
		return Arg {
			.arg = .{
				.name = eq.left,
				.side = eq.right
			}
		};
	}
	return Arg {
		.name=try parse_name(mem, i, text, err)
	};
}

pub fn parse_name(mem: *const std.mem.Allocator, i: *u64, text: []u8, err: *Buffer(Error)) ParseError!Name {
	try skip_whitespace(mem, i, text, err);
	var c = text[i.*];
	if (c == '"'){
		i.* += 1;
		const start = i.*;
		while (i.* < text.len){
			if (i.* == '"'){
				return Name {
					.literal = .{
						.text = text[start .. i.*],
						.pos = start
					}
				};
			}
			i.* += 1;
		}
		err.append(set_error(mem, i.*, "Encountered end of file in literal parse\n", .{}))
			catch unreachable;
		return ParseError.UnexpectedEOF;
	}
	const start = i.*;
	while (i.* < text.len){
		if (i.* == ' ' or i.* == '\t' or t.* == '\n'){
			return Name {
				.name = .{
					.text = text[start .. i.*],
					.pos = start
				}
			};
		}
		i.* += 1;
	}
	err.append(set_error(mem, i.*, "Encountered end of file in name parse\n", .{}))
		catch unreachable;
	return ParseError.UnexpectedEOF;
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
	program: Buffer(Bind),
	tree: Buffer(Node),
	input_q: Q,
	pub fn init(mem: *const std.mem.Allocator) VAST {
		return VAST{
			.program=Buffer(Bind).init(mem.*),
			.tree=Buffer(Node).init(mem.*),
			.input_q=Q.init(mem)
		};
	}
};

const Q = struct {
	head: ?*QNode,
	tail: ?*QNode,
	mem: *const std.mem.Allocator,

	pub fn init(mem: *const std.mem.Allocator) Q {
		return Q{
			.head = null,
			.tail = null,
			.mem = mem
		};
	}

	pub fn push(self: *Q, node: Buffer(Buffer(Part))) void {
		if (self.head == null){
			const n = self.mem.create(QNode)
				catch unreachable;
			n.* = QNode{
				.val=node,
				.next=null
			};
			self.head = n;
			self.tail = n;
		}
		const n = self.mem.create(QNode)
			catch unreachable;
		n.* = QNode{
			.val=node,
			.next=null,
		};
		if (self.tail) |tail| {
			tail.next = n;
			self.tail = n;
		}
	}
	
	pub fn pop(self: *Q) ?Buffer(Buffer(Part)){
		if (self.head == null) {
			return null;
		}
		if (self.head) |qnode| {
			const node = qnode.val;
			self.head = qnode.next;
			return node;
		}
		return null;
	}
};

const QNode = struct {
	val: Buffer(Buffer(Part)),
	next :?*QNode
};

const Node  = struct {
	name: Buffer(Part),
	rules: Buffer(*Bind),
	chunk: Buffer(Bind),
	structure: Buffer(Buffer(Buffer(Part)))
};

pub fn compare_part(left: Part, right: Part) bool {
	switch (left){
		.literal => {
			if (right != .literal){
				return false;
			}
			return std.mem.eql(u8, left.literal.text, right.literal.text);
		},
		.ref => {
			if (right != .ref){
				return false;
			}
			return std.mem.eql(u8, left.ref.name, right.ref.name);
		},
		.compref => {
			if (right != .compref){
				return false;
			}
			if (left.compref.right != null and right.compref.right == null){
				return false;
			}
			if (left.compref.right == null and right.compref.right != null){
				return false;
			}
			if (left.compref.right) |r| {
				return compare_alt(left.compref.ref.*, right.compref.ref.*) and compare_alt(r.*, right.compref.right.?.*);
			}
			return compare_alt(left.compref.ref.*, right.compref.ref.*);
		}
	}
}

pub fn compare_alt(left: Buffer(Part), right: Buffer(Part)) bool {
	if (left.items.len != right.items.len){
		return false;
	}
	for (left.items, right.items) |l, r| {
		if (compare_part(l, r) == false){
			return false;
		}
	}
	return true;
}

pub fn compare_side(left: Buffer(Buffer(Part)), right: Buffer(Buffer(Part))) bool {
	if (left.items.len != right.items.len){
		return false;
	}
	for (left.items, right.items) |l, r| {
		if (compare_alt(l, r) == false){
			return false;
		}
	}
	return true;
}

pub fn create_node(mem: *const std.mem.Allocator, vast: *VAST, bind: *Bind, _: *Buffer(Error)) void {
	outer: for (bind.left.items) |left| {
		for (vast.tree.items) |*node| {
			if (compare_alt(node.name, left)){
				node.rules.append(bind)
					catch unreachable;
				continue :outer;
			}
		}
		var node = Node{
			.name=left,
			.rules = Buffer(*Bind).init(mem.*),
			.structure = Buffer(Buffer(Buffer(Part))).init(mem.*),
			.chunk=Buffer(Bind).init(mem.*)
		};
		node.rules.append(bind)
			catch unreachable;
		vast.tree.append(node)
			catch unreachable;
	}
}

//TODO validity checks
pub fn add_binds(mem: *const std.mem.Allocator, vast: *VAST, subbinds: Buffer(Bind), err: *Buffer(Error)) LinkError!void {
	//NOTE expects applied bind variables
	const old_len = vast.program.items.len;
	vast.program.appendSlice(subbinds.items)
		catch unreachable;
	for (old_len..vast.program.items.len) |i| {
		create_node(mem, vast, &vast.program.items[i], err);
	}
	for (old_len..vast.program.items.len) |i| {
		execute_bind(vast, &vast.program.items[i], err);
	}
}

pub fn show_vast(vast: VAST) void {
	show_program(vast.program);
	for (vast.tree.items) |node| {
		show_node(node);
	}
}

pub fn show_node(node: Node) void {
	std.debug.print("Node [\n", .{});
	std.debug.print("Name: ", .{});
	for (node.name.items) |part| {
		show_part(part);
	}
	std.debug.print("\nRules:", .{});
	for (node.rules.items) |rule| {
		show_bind(rule.*);
	}
	std.debug.print("\nStructure: ", .{});
	for (node.structure.items) |s| {
		for (s.items, 0..) |alt, i| {
			if (i > 0){
				std.debug.print("| ", .{});
			}
			for (alt.items) |part| {
				show_part(part);
			}
		}
	}
	std.debug.print("\n]\n\n", .{});
}

pub fn find_node(vast: *VAST, bind: *Bind) ?*Node {
	for (vast.tree.items) |*node| {
		if (compare_alt(node.name, bind.left)){
			return node;
		}
	}
	return null;
}

pub fn execute_bind(vast: *VAST, bind: *Bind, _: *Buffer(Error)) void {
	if (bind.right) |right| {
		for (bind.left.items) |alt| {
			for (vast.tree.items) |*node| {
				if (compare_alt(alt, node.name)){
					if (bind.subbinds) |subs| {
						node.chunk.appendSlice(subs.items)
							catch unreachable;
					}
					node.structure.append(right)
						catch unreachable;
				}
			}
		}
		return;
	}
	//unbind
	outer: for (bind.left.items) |alt| {
		for (vast.tree.items, 0..) |node, i| {
			if (compare_alt(node.name, alt)){
				_ = vast.tree.swapRemove(i);
				continue :outer;
			}
		}
	}
}

const ArgPair = struct {
	name: Buffer(Part),
	val: Part,
	def_type: Buffer(Part)
};

pub fn scrape_args(candidate_name: *Buffer(Part), node_name: *Buffer(Part), argmap: *Buffer(ArgPair)) void {
	for (candidate_name.items, node_name.items) |is_arg, arg_val| {
		if (is_arg == .compref){
			if (is_arg.compref.right) |arg_type| {
				argmap.append(ArgPair{
					.name=is_arg.compref.ref.*,
					.val=arg_val,
					.def_type=arg_type.*
				}) catch unreachable;
			}
			else{
				if (arg_val.compref.right) |right| {
					scrape_args(is_arg.compref.ref, right, argmap);
				}
				else{
					std.debug.assert(false);
				}
			}
		}
	}
}

pub fn expand_args(mem: *const std.mem.Allocator, expansion: *Buffer(Part), argmap: *Buffer(ArgPair)) Buffer(Part) {
	var new = Buffer(Part).init(mem.*);
	outer: for (expansion.items) |part| {
		switch (part){
			.literal => {
				new.append(part)
					catch unreachable;
			},
			.ref => {
				for (argmap.items) |arg| {
					if (arg.name.items.len == 1){
						if (compare_part(arg.name.items[0], part)){
							new.append(arg.val)
								catch unreachable;
							continue :outer;
						}
					}
				}
				new.append(part)
					catch unreachable;
			},
			.compref => {
				if (part.compref.right) |right| {
					const synth = Part{
						.compref = .{
							.ref = mem.create(Buffer(Part)) catch unreachable,
							.right = mem.create(Buffer(Part)) catch unreachable,
							.pos = part.compref.pos
						}
					};
					synth.compref.ref.* = expand_args(mem, part.compref.ref, argmap);
					synth.compref.right.?.* = expand_args(mem, right, argmap);
					new.append(synth)
						catch unreachable;
				}
				else{
					for (argmap.items) |arg| {
						if (compare_alt(arg.name, part.compref.ref.*)){
							new.append(arg.val)
								catch unreachable;
							continue :outer;
						}
					}
					const synth = Part{
						.compref = .{
							.ref=mem.create(Buffer(Part)) catch unreachable,
							.right=null,
							.pos=part.compref.pos
						}
					};
					synth.compref.ref.* = expand_args(mem, part.compref.ref, argmap);
					new.append(synth)
						catch unreachable;
				}
			}
		}
	}
	return new;
}

pub fn run_instantiations(mem: *const std.mem.Allocator, vast: *VAST, node: Buffer(Buffer(Part)), _: *Buffer(Error), chunk: *Buffer(Bind)) LinkError!void {
	outer: for (node.items) |*alt| {
		for (vast.tree.items) |*candidate| {
			if (compare_alt(alt.*, candidate.name)){
				for (candidate.structure.items) |expansion| {
					chunk.appendSlice(candidate.chunk.items)
						catch unreachable;
					var argmap = Buffer(ArgPair).init(mem.*);
					defer argmap.deinit();
					scrape_args(&candidate.name, alt, &argmap);
					if (expansion.items.len == 1){
						const structure = expand_args(mem, &expansion.items[0], &argmap);
						var wrapper = Buffer(Buffer(Part)).init(mem.*);
						wrapper.append(structure)
							catch unreachable;
						vast.input_q.push(wrapper);
					}
				}
				continue :outer;
			}
		}
	}
}
