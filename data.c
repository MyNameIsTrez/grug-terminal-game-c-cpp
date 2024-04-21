#include "data.h"

#include <stdlib.h>
#include <string.h>

struct data data;

void init_data() {
	memset(&data, 0, sizeof(data));

	data.gold = 40;
}

void free_data() {
	free(data.human_globals[0]);
	free(data.human_globals[1]);

	free(data.tool_globals[0]);
	free(data.tool_globals[1]);
}
