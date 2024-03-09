#include "human.h"

#include "data.h"
#include "typedefs.h"

human get_human(id id) {
	// TODO: Don't always return [0]!!
	(void)id;
	return data.humans[0];
}

static f64 min_f64(f64 a, f64 b) {
	if (a < b) {
		return a;
	}
	return b;
}

void change_human_health(id id, f64 health) {
	// TODO: Don't always modify [0]!!
	(void)id;
	human *h = &data.humans[0];
	h->health = min_f64(h->health + health, h->max_health);
}
