#include "data.h"
#include "game/tool.h"
#include "grug.h"
#include "mod.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <unistd.h>

typedef void (*on_tool_use_fn)(tool tool);
typedef human (*define_human_fn)();
typedef tool (*define_tool_fn)();

static void handle_poison(human *human) {
	if (human->poison.ticks_left > 0) {
		change_human_health(human->id, -human->poison.damage_per_tick);
		human->poison.ticks_left--;
	}
}

static void get_fns_impl(mod_directory dir, char *fn_name) {
	for (size_t i = 0; i < dir.dirs_size; i++) {
		get_fns_impl(dir.dirs[i], fn_name);
	}
	for (size_t i = 0; i < dir.files_size; i++) {
		void *fn = dlsym(dir.files[i].dll, fn_name);
		if (fn) {
			data.fns[data.fn_count++] = fn;
		}
	}
}

static void *get_fns(char *fn_name) {
	data.fn_count = 0;
	get_fns_impl(data.mods, fn_name);
	return data.fns;
}

static void fight() {
	human *player = &data.humans[0];
	human *opponent = &data.humans[1];

	tool *player_tool = &data.tools[0];
	tool *opponent_tool = &data.tools[1];

	printf("You have %d health\n", player->health);
	printf("The opponent has %d health\n\n", opponent->health);

	player->opponent_id = 1;
	opponent->opponent_id = 0;

	on_tool_use_fn *on_tool_use_array = get_fns("on_tool_use");

	printf("You use your %s\n", player_tool->name);
	sleep(1);
	on_tool_use_array[player_tool->id](*player_tool);
	sleep(1);

	handle_poison(opponent);
	if (opponent->health == 0) {
		printf("The opponent died!\n");
		sleep(1);
		data.state = STATE_PICKING_PLAYER;
		data.gold += opponent->kill_gold_value;
		return;
	}

	printf("The opponent uses their %s\n", opponent_tool->name);
	sleep(1);
	on_tool_use_array[opponent_tool->id](*opponent_tool);
	sleep(1);

	handle_poison(player);
	if (player->health == 0) {
		printf("You died!\n");
		sleep(1);
		data.state = STATE_PICKING_PLAYER;
		return;
	}
}

static void discard_unread() {
	int c;
	while ((c = getchar()) != '\n' && c != EOF) {}
}

// Returns true if the input was valid
static bool read_size(size_t *output) {
	char buffer[42];
	if (!fgets(buffer, sizeof(buffer), stdin)) {
		perror("fgets");
		exit(EXIT_FAILURE);
	}

	char *endptr;
	errno = 0;
	long l = strtol(buffer, &endptr, 10);
	if (errno != 0) {
		perror("strtol");
		// This is to prevent the next strtol() call from continuing
		// when the input was for example a long series of "11111111..."
		discard_unread();
		return false;
	} else if (buffer == endptr) {
		fprintf(stderr, "No number was provided\n");
		return false;
	} else if (*endptr != '\n' && *endptr != '\0') {
		fprintf(stderr, "There was an extra character after the number\n");
		return false;
	} else if (l < 0) {
		fprintf(stderr, "You can't enter a negative number\n");
		return false;
	}

	*output = l;

	return true;
}

static void print_opponent_humans() {
	define_human_fn *define_human_array = get_fns("define_human");

	for (size_t i = 0; i < data.fn_count; i++) {
		human human = define_human_array[i]();
		printf("%ld. %s, worth %d gold when killed\n", i + 1, human.name, human.kill_gold_value);
	}
	printf("\n");
}

static void pick_opponent() {
	printf("You have %d gold\n\n", data.gold);

	print_opponent_humans();

	printf("Type the number of the human you want to fight:\n");

	size_t opponent_number;
	if (!read_size(&opponent_number)) {
		return;
	}

	if (opponent_number == 0) {
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (opponent_number > data.fn_count) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.fn_count);
		return;
	}

	size_t opponent_index = opponent_number - 1;

	define_human_fn *define_human_array = get_fns("define_human");
	human human = define_human_array[opponent_index]();

	human.id = 1;
	human.max_health = human.health;

	data.humans[1] = human;

	// Give the opponent a random tool
	define_tool_fn *define_tool_array = get_fns("define_tool");
	size_t tool_index = rand() % data.fn_count;
	tool tool = define_tool_array[tool_index]();

	tool.id = tool_index;
	tool.human_parent_id = 1;

	data.tools[1] = tool;

	data.state = STATE_FIGHTING;
}

static void print_tools() {
	define_tool_fn *define_tool_array = get_fns("define_tool");

	for (size_t i = 0; i < data.fn_count; i++) {
		tool tool = define_tool_array[i]();
		printf("%ld. %s costs %d gold\n", i + 1, tool.name, tool.buy_gold_value);
	}
	printf("\n");
}

static void pick_tools() {
	printf("You have %d gold\n\n", data.gold);

	print_tools();

	printf("Type the number of any tools you want to buy%s:\n", data.player_has_tool ? " (type 0 to skip)" : "");

	size_t tool_number;
	if (!read_size(&tool_number)) {
		return;
	}

	if (tool_number == 0) {
		if (data.player_has_tool) {
			data.state = STATE_PICKING_OPPONENT;
			return;
		}
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (tool_number > data.fn_count) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.fn_count);
		return;
	}

	size_t tool_index = tool_number - 1;

	define_tool_fn *define_tool_array = get_fns("define_tool");
	tool tool = define_tool_array[tool_index]();

	if (tool.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to buy that tool\n");
		return;
	}

	data.gold -= tool.buy_gold_value;

	tool.id = tool_index;
	tool.human_parent_id = 0;

	data.tools[0] = tool;

	data.player_has_tool = true;
}

static void print_playable_humans() {
	define_human_fn *define_human_array = get_fns("define_human");

	for (size_t i = 0; i < data.fn_count; i++) {
		human human = define_human_array[i]();
		printf("%ld. %s, costing %d gold\n", i + 1, human.name, human.buy_gold_value);
	}
	printf("\n");
}

static void pick_player() {
	printf("You have %d gold\n\n", data.gold);

	print_playable_humans();

	printf("Type the number of the human you want to play as%s:\n", data.player_has_human ? " (type 0 to skip)" : "");

	size_t player_number;
	if (!read_size(&player_number)) {
		return;
	}

	if (player_number == 0) {
		if (data.player_has_human) {
			data.state = STATE_PICKING_TOOLS;
			return;
		}
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (player_number > data.fn_count) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.fn_count);
		return;
	}

	size_t player_index = player_number - 1;

	define_human_fn *define_human_array = get_fns("define_human");
	human human = define_human_array[player_index]();

	if (human.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to pick that human\n");
		return;
	}

	data.gold -= human.buy_gold_value;

	human.id = 0;
	human.max_health = human.health;

	data.humans[0] = human;

	data.player_has_human = true;

	data.state = STATE_PICKING_TOOLS;
}

static void update() {
	switch (data.state) {
	case STATE_PICKING_PLAYER:
		pick_player();
		break;
	case STATE_PICKING_TOOLS:
		pick_tools();
		break;
	case STATE_PICKING_OPPONENT:
		pick_opponent();
		break;
	case STATE_FIGHTING:
		fight();
		break;
	}
}

int main() {
	srand(time(NULL)); // Seed the random number generator with the number of seconds since 1970

	init_data();

	while (true) {
		grug_free_mods(data.mods);

		data.mods = grug_reload_modified_mods("mods", "mods", "dlls");

		// grug_print_mods(data.mods);

		update();

		printf("\n");

		sleep(1);
	}
	
	free_data();
}
