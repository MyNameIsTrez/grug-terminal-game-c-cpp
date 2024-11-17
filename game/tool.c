#include "tool.h"

#include "data.h"
#include "typedefs.h"

#include <assert.h>

id game_fn_get_human_parent(id tool_id) {
	assert(tool_id < 2);
	return data.tools[tool_id].human_parent_id;
}
