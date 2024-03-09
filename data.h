#pragma once

#include "game/human.h"
#include "grug.h"

#include <stdbool.h>

struct data {
	struct mod_directory mods;
	enum {
		STATE_PICKING_HUMANS,
		STATE_PICKING_TOOLS,
		STATE_FIGHTING,
	} state;
	struct human humans[2];
	void *fns;
	size_t fn_count;
};

extern struct data data;

void init_data();
void free_data();
