#include "mod.h"

human define_human() {
	return (human){
		.name = "Child",
		.health = 5,
		.buy_gold_value = 5,
		.kill_gold_value = 1,
	};
}
