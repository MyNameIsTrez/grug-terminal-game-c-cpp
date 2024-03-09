#pragma once

#include "typedefs.h"

struct human {
	string name;
	f64 health;
	i32 buy_gold_value;
	i32 kill_gold_value;

	// These should not be initialized by mods
	id id;
	f64 max_health;
};
