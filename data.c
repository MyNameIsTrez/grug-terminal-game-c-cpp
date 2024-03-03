#include "data.h"

#include "game/human.h"

#include <stdlib.h>

void init_data() {
	data.humans = calloc(2, sizeof(struct human));
}

void free_data() {
	free(data.humans);
}
