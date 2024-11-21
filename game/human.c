#include "human.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>

id game_fn_get_opponent(id human_id) {
	assert(human_id < 2);
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

void game_fn_change_human_health(id id, i32 added_health) {
	assert(id < 2);
	human *h = &data.humans[id];
	h->health = clamp_i32(h->health + added_health, 0, h->max_health);
}
