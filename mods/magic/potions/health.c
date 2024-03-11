#include "mod.h"

tool define_tool() {
	return (tool){
		.name = "Health potion",
		.buy_gold_value = 5,
	};
}

void on_tool_use(tool self) {
	change_human_health(get_human(self.human_parent_id).id, 30);
}
