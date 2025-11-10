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
