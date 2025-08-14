#include "human.h"

#include "data.h"
#include "macros.h"
#include "typedefs.h"

#include <assert.h>
#include <stdio.h>

id game_fn_get_opponent(id human_id) {
	if (human_id >= 2) {
		GAME_FUNCTION_ERROR("get_opponent(): the human_id argument was %zu, while the function only expects it to be up to 2", human_id);
		return -1;
	}
	return data.humans[human_id].opponent_id;
}

static i32 min_i32(i32 a, i32 b) {
	return a < b ? a : b;
}

static i32 max_i32(i32 a, i32 b) {
	return a > b ? a : b;
}

static i32 clamp_i32(i32 n, i32 lowest, i32 highest) {
	return max_i32(lowest, min_i32(highest, n));
}

void game_fn_change_human_health(id human_id, i32 added_health) {
	if (human_id >= 2) {
		GAME_FUNCTION_ERROR("change_human_health(): the human_id argument was %zu, while the function only expects it to be up to 2", human_id);
		return;
	}
	human *h = &data.humans[human_id];
	h->health = clamp_i32(h->health + added_health, 0, h->max_health);
}
