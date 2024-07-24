#pragma once

#include "typedefs.h"

human get_human(i32 id);
void change_human_health(i32 id, i32 health);

struct poison {
	i32 ticks_left;
	i32 damage_per_tick;
};

struct human {
	string name;
	i32 health;
	i32 buy_gold_value;
	i32 kill_gold_value;
	// poison poison; // TODO: Support poison again

	// These are not initialized by mods
	i32 id;
	i32 opponent_id;
	i32 max_health;
};
