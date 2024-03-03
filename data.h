#pragma once

#include <stdbool.h>

struct data {
	bool fighting;
	struct human *humans;
};

extern struct data data;

void init_data();
void free_data();
