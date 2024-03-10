#include "mod.h"

human define_human() {
	return (human){
		.name = "Caveman",
		.health = 30,
		.buy_gold_value = 15,
		.kill_gold_value = 10,
	};
}
