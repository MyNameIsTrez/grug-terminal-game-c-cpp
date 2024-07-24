#pragma once

#include "mod.h"
#include "typedefs.h"

// void on_tool_use(void *globals, tool self); // TODO: BRING THIS BACK
void on_tool_use(i32 self);

struct tool {
	string name;
	i32 buy_gold_value;

	// These are not initialized by mods
	i32 human_parent_id;
	tool_on_fns *on_fns;
};

struct tool_on_fns {
	typeof(on_tool_use) *use;
};
