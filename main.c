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

static void get_fns(struct mod_directory dir, char *fn_name) {
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

	typedef struct tool (*define_tools)();
	define_tools *define_tools_array = (void *)data.fns;

	for (size_t i = 0; i < data.fn_count; i++) {
		struct tool tool = define_tools_array[i]();
		printf("%s costs %d\n", tool.name, tool.cost);
	}
	printf("\n");
}

// Returns true if the input was valid
static bool read_long(long *output) {
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
		exit(EXIT_FAILURE);
	} else if (buffer == endptr) {
		fprintf(stderr, "No number was provided\n");
		return false;
	} else if (*endptr != '\n' && *endptr != '\0') {
		fprintf(stderr, "There was an extra character after the number\n");
		return false;
	}

	*output = l;

	return true;
}

static void pick_humans() {
	printf("In pick_humans()\n");

	printf("Type the index of the human that should fight in team 1:\n");
	size_t human1_index;
	if (!read_long(&human1_index)) {
		return;
	}

	printf("Type the index of the human that should fight in team 2:\n");
	size_t human2_index;
	if (!read_long(&human2_index)) {
		return;
	}
	
	printf("human1_index: %ld\n", human1_index);
	printf("human2_index: %ld\n", human2_index);

	data.state = STATE_PICKING_TOOLS;
}

static void update() {
	printf("In update()\n");

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

	printf("Hello, grug!\n");

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
