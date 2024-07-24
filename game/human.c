#include "human.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>
#include <stdio.h>

human get_human(i32 id) {
	assert(id >= 0 && id < 2);
	return data.humans[id];
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

void change_human_health(i32 id, i32 health) {
	assert(id >= 0 && id < 2);
	human *h = &data.humans[id];

	h->health = min_i32(h->health + health, h->max_health);
	h->health = max_i32(h->health, 0);

	// printf("Human with ID %d now has %d health\n", id, h->health);
}
