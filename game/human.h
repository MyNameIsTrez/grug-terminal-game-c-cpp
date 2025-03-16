#pragma once

#include "typedefs.h"

void on_human_spawn(void *globals);
void on_human_despawn(void *globals);

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
	human_on_fns *on_fns;
};

struct human_on_fns {
	typeof(on_human_spawn) *spawn;
	typeof(on_human_despawn) *despawn;
};
