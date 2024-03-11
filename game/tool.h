#pragma once

#include "typedefs.h"

struct tool {
	string name;
	i32 buy_gold_value;

	// These should not be initialized by mods
	id id;
	i32 human_parent_id;
};
