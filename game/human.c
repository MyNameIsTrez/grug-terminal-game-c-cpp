#include "human.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>

human get_human(id id) {
	assert(id >= 0 && id < 2);
	return data.humans[id];
}

static f64 min_f64(f64 a, f64 b) {
	if (a < b) {
		return a;
	}
	return b;
}

void change_human_health(id id, f64 health) {
	assert(id >= 0 && id < 2);
	human *h = &data.humans[id];
	h->health = min_f64(h->health + health, h->max_health);
}
