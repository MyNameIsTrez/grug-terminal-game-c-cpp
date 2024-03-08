#include "mod.h"

struct tool define_tool() {
	return (struct tool){
		.name = "Health potion",
		.monetary_value = 5,
	};
}

void on_use() {
	set_human_health(min_i32(get_human_health() + 30, get_human_max_health()));
}
