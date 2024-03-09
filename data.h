#pragma once

#include "game/human.h"
#include "grug.h"
#include "typedefs.h"

#include <stdbool.h>

struct data {
	mod_directory mods;
	enum {
		STATE_PICKING_HUMANS,
		STATE_PICKING_TOOLS,
		STATE_FIGHTING,
	} state;
	human humans[2];
	void **fns;
	size_t fn_count;
	i32 gold;
};

extern struct data data;

void init_data();
void free_data();
