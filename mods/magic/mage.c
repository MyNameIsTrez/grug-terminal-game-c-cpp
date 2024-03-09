#include "mod.h"

human define_human() {
	return (human){
		.name = "Mage",
		.health = 20,
		.buy_gold_value = 200,
		.kill_gold_value = 50,
	};
}
