#pragma once

#include "typedefs.h"

struct tool {
	string name;
	i32 gold_cost;

	// These should not be initialized by mods
	id id;
	i32 human_parent_id;
};
