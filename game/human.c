#include "human.h"

#include "data.h"
#include "typedefs.h"

static struct human *get_human() {
	return &data.humans[0];
}

f64 get_human_health() {
	struct human *human = get_human();
	return human->health;
}

f64 get_human_max_health() {
	struct human *human = get_human();
	return human->max_health;
}

void set_human_health(f64 health) {
	struct human *human = get_human();
	human->health = health;
}
