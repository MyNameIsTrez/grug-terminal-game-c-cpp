#include "mod.h"

tool define_tool() {
	return (tool){
		.name = "Club",
		.buy_gold_value = 1,
	};
}

void on_tool_use(tool self) {
	change_human_health(get_human(self.human_parent_id).opponent_id, -8);
}
