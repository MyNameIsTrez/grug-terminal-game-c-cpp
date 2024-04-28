#pragma once

// grug mods always require these
#include <string.h> // size_t, memcpy()

#include "typedefs.h"

#include "game/human.h"
#include "game/tool.h"

int printf(const char *restrict format, ...);

struct about {
	char *name;
    char *version;
	char *author;
};
