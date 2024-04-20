#include "data.h"

#include <stdlib.h>
#include <string.h>

struct data data;

void init_data() {
	memset(&data, 0, sizeof(data));

	data.gold = 40;
}

void free_data() {
	grug_free_mods(mods);
}
