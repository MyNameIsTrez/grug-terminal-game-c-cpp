#include "data.h"
#include "game/tool.h"
#include "grug.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

typedef human (*define_human)();
typedef tool (*define_tools)();

static void fight() {
	printf("In fight()\n");
}

static void get_fns_recursive(mod_directory dir, char *fn_name) {
	for (size_t i = 0; i < dir.dirs_size; i++) {
		get_fns_recursive(dir.dirs[i], fn_name);
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
	get_fns_recursive(data.mods, fn_name);
	return data.fns;
}

static void pick_tools() {
	printf("In pick_tools()\n");

	define_tools *define_tools_array = get_fns("define_tool");

	for (size_t i = 0; i < data.fn_count; i++) {
		tool tool = define_tools_array[i]();
		printf("%s costs %d gold\n", tool.name, tool.gold_cost);
	}
	printf("\n");
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
	define_human *define_human_array = get_fns("define_human");

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
	// printf("opponent_index: %ld\n", opponent_index);

	define_human *define_human_array = get_fns("define_human");
	human human = define_human_array[opponent_index]();

	data.humans[1] = human;

	data.state = STATE_PICKING_TOOLS;
}

static void print_playable_humans() {
	define_human *define_human_array = get_fns("define_human");

	for (size_t i = 0; i < data.fn_count; i++) {
		human human = define_human_array[i]();
		printf("%ld. %s, costing %d gold\n", i + 1, human.name, human.buy_gold_value);
	}
	printf("\n");
}

static void pick_player() {
	printf("You have %d gold\n\n", data.gold);

	print_playable_humans();

	printf("Type the number of the human you want to play as:\n");

	size_t player_number;
	if (!read_size(&player_number)) {
		return;
	}

	if (player_number == 0) {
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (player_number > data.fn_count) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.fn_count);
		return;
	}

	size_t player_index = player_number - 1;
	// printf("player_index: %ld\n", player_index);

	define_human *define_human_array = get_fns("define_human");
	human human = define_human_array[player_index]();

	if (human.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to pick that human\n");
		return;
	}

	data.humans[0] = human;

	data.state = STATE_PICKING_OPPONENT;
}

static void update() {
	switch (data.state) {
	case STATE_PICKING_PLAYER:
		pick_player();
		break;
	case STATE_PICKING_OPPONENT:
		pick_opponent();
		break;
	case STATE_PICKING_TOOLS:
		pick_tools();
		break;
	case STATE_FIGHTING:
		fight();
		break;
	}
}

int main() {
	init_data();

	while (true) {
		grug_free_mods(data.mods);

		data.mods = grug_reload_modified_mods("mods", "mods", "dlls");

		// grug_print_mods(data.mods);
		// printf("\n");

		update();

		sleep(1);
	}
	
	free_data();
}
