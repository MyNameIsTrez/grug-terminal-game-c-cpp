#include "mod.h"

tool define_tool() {
	return (tool){
		.name = "Health potion",
		.gold_cost = 5,
	};
}

void on_use(tool self) {
	change_human_health(get_human(self.human_parent_id).id, 30);
}
