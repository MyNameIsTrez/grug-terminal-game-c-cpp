#pragma once

#include "mod.h"
#include "typedefs.h"

void on_tool_use(void *globals, tool self);

struct tool {
	string name;
	i32 buy_gold_value;

	// These should not be initialized by mods
	i32 human_parent_id;
	tool_on_fns *on_fns;
};

struct tool_on_fns {
	typeof(on_tool_use) *use;
};
