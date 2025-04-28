#include "tool.h"

#include "data.h"
#include "macros.h"
#include "typedefs.h"

#include <assert.h>
#include <stdio.h>

id game_fn_get_human_parent(id tool_id) {
	if (tool_id >= 2) {
		GAME_FUNCTION_ERROR("get_human_parent(): the tool_id argument was %zu, while the function only expects it to be up to 2", tool_id);
		return -1;
	}
	return data.tools[tool_id].human_parent_id;
}
