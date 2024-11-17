#pragma once

#include "typedefs.h"

human get_human(id id);
void change_human_health(id id, i32 health);

struct human {
	string name;
	i32 health;
	i32 buy_gold_value;
	i32 kill_gold_value;

	// These are not initialized by mods
	id id;
	id opponent_id;
	i32 max_health;
};
