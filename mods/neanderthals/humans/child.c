#include "mod.h"

human define_human() {
	return (human){
		.name = "Child",
		.health = 3,
		.buy_gold_value = 4,
		.kill_gold_value = 2,
	};
}
