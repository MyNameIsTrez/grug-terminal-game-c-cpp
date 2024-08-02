#include "tool.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>

i32 game_fn_get_human_parent(i32 tool_id) {
	assert(tool_id >= 0 && tool_id < 2);
	return data.tools[tool_id].human_parent_id;
}
