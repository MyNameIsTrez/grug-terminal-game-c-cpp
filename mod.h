#pragma once

// grug mods always require these
#include <string.h> // size_t, memcpy()

#include "typedefs.h"

#include "game/human.h"
#include "game/tool.h"

struct about {
	char *name;
    char *version;
	char *author;
};

tool define_tool();
human define_human();

void on_tool_use(tool self);

human get_human(id id);
void change_human_health(id id, i32 health);
