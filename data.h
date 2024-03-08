#pragma once

#include "grug.h"

#include <stdbool.h>

struct data {
	struct mod_directory mods;
	bool fighting;
	struct human *humans;
};

extern struct data data;

void init_data();
void free_data();
