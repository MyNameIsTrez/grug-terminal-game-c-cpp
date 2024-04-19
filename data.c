#include "data.h"

#include <stdlib.h>

struct data data;

void init_data() {
	data.fns = calloc(420, sizeof(void *));
	data.gold = 40;
}

void free_data() {
	grug_free_mods(mods);
	free(data.fns);
}
