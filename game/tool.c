#include "tool.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>
#include <stdio.h>

id game_fn_get_human_parent(id tool_id) {
	if (tool_id >= 2) {
		fprintf(stderr, "grug runtime error in %s(): the tool_id argument of get_human_parent() was %zu, while the function only expects it to be up to 2, in %s\n", grug_on_fn_name, tool_id, grug_on_fn_path);
		return -1;
	}
	return data.tools[tool_id].human_parent_id;
}
