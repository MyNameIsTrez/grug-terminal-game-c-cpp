#include "human.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>

i32 game_fn_get_opponent(i32 human_id) {
	assert(human_id >= 0 && human_id < 2);
	return data.humans[human_id].opponent_id;
}

static i32 min_i32(i32 a, i32 b) {
	if (a < b) {
		return a;
	}
	return b;
}

static i32 max_i32(i32 a, i32 b) {
	if (a > b) {
		return a;
	}
	return b;
}

void game_fn_change_human_health(i32 id, i32 added_health) {
	assert(id >= 0 && id < 2);
	human *h = &data.humans[id];

	h->health = min_i32(h->health + added_health, h->max_health);
	h->health = max_i32(h->health, 0);
}
