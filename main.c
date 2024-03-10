#include "data.h"
#include "game/tool.h"
#include "grug.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

static void fight() {
	printf("In fight()\n");
}

static void get_fns(mod_directory dir, char *fn_name) {
	for (size_t i = 0; i < dir.dirs_size; i++) {
		get_fns(dir.dirs[i], fn_name);
	}
	for (size_t i = 0; i < dir.files_size; i++) {
		void *fn = dlsym(dir.files[i].dll, fn_name);
		if (fn) {
			data.fns[data.fn_count++] = fn;
		}
	}
}

static void pick_tools() {
	printf("In pick_tools()\n");

	data.fn_count = 0;
	get_fns(data.mods, "define_tool");

	typedef tool (*define_tools)();
	define_tools *define_tools_array = (void *)data.fns;

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

static void print_humans() {
	data.fn_count = 0;
	get_fns(data.mods, "define_human");

	typedef human (*define_human)();
	define_human *define_human_array = (void *)data.fns;

	for (size_t i = 0; i < data.fn_count; i++) {
		human human = define_human_array[i]();
		printf("%ld. %s costs %d gold\n", i + 1, human.name, human.buy_gold_value);
	}
	printf("\n");
}

static void pick_humans() {
	printf("You have %d gold\n\n", data.gold);

	print_humans();

	printf("Type the number of the human to fight:\n");

	size_t opponent_number;
	if (!read_size(&opponent_number)) {
		return;
	}

	printf("opponent_number: %ld\n", opponent_number);

	if (opponent_number == 0) {
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (opponent_number > data.fn_count) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.fn_count);
		return;
	}

	size_t opponent_index = opponent_number - 1;

	printf("opponent_index: %ld\n", opponent_index);

	data.state = STATE_PICKING_TOOLS;
}

static void update() {
	switch (data.state) {
	case STATE_PICKING_HUMANS:
		pick_humans();
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
