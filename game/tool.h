#pragma once

#include "typedefs.h"

void on_tool_use(void *globals);

struct tool {
	string name;
	i32 buy_gold_value;

	// These are not initialized by mods
	id human_parent_id;
	tool_on_fns *on_fns;
};

struct tool_on_fns {
	typeof(on_tool_use) *use;
};
