#pragma once

#include "game/human.h"
#include "game/tool.h"
#include "grug.h"
#include "typedefs.h"

#include <stdbool.h>

struct data {
	mod_directory mods;
	enum {
		STATE_PICKING_PLAYER,
		STATE_PICKING_TOOLS,
		STATE_PICKING_OPPONENT,
		STATE_FIGHTING,
	} state;
	void **fns;
	size_t fn_count;
	i32 gold;
	human humans[2];
	tool tools[2];
};

extern struct data data;

void init_data();
void free_data();
