#pragma once

#include "game/human.h"
#include "game/tool.h"
#include "grug.h"
#include "typedefs.h"

#include <stdbool.h>

#define MAX_TYPE_FILES 420420

#define PLAYER_INDEX 0
#define OPPONENT_INDEX 1

struct data {
	enum {
		STATE_PICKING_PLAYER,
		STATE_PICKING_TOOLS,
		STATE_PICKING_OPPONENT,
		STATE_FIGHTING,
	} state;
	struct grug_file type_files[MAX_TYPE_FILES];
	size_t type_files_size;
	i32 gold;

	human humans[2];
	void *human_dlls[2];
	void *human_globals[2];

	tool tools[2];
	void *tool_dlls[2];
	void *tool_globals[2];

	bool player_has_human;
	bool player_has_tool;
};

extern struct data data;

void init_data();
void free_data();
