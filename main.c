#include "data.h"
#include "game/tool.h"
#include "grug.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

static struct human human_definition;
static struct tool tool_definition;

void game_fn_define_human(string name, i32 health, i32 buy_gold_value, i32 kill_gold_value) {
	human_definition = (struct human){
		.name = name,
		.health = health,
		.buy_gold_value = buy_gold_value,
		.kill_gold_value = kill_gold_value,
	};
}

void game_fn_define_tool(string name, i32 buy_gold_value) {
	tool_definition = (struct tool){
		.name = name,
		.buy_gold_value = buy_gold_value,
	};
}

static void push_file_containing_fn(struct grug_file file) {
	if (data.type_files_size + 1 > MAX_TYPE_FILES) {
		fprintf(stderr, "There are more than %d files containing the requested type, exceeding MAX_TYPE_FILES", MAX_TYPE_FILES);
		exit(EXIT_FAILURE);
	}
	data.type_files[data.type_files_size++] = file;
}

static void get_type_files_impl(struct grug_mod_dir dir, char *fn_name) {
	for (size_t i = 0; i < dir.dirs_size; i++) {
		get_type_files_impl(dir.dirs[i], fn_name);
	}
	for (size_t i = 0; i < dir.files_size; i++) {
		if (strcmp(fn_name, dir.files[i].define_type) == 0) {
			push_file_containing_fn(dir.files[i]);
		}
	}
}

static struct grug_file *get_type_files(char *fn_name) {
	data.type_files_size = 0;
	get_type_files_impl(grug_mods, fn_name);
	return data.type_files;
}

static void fight(void) {
	human *player = &data.humans[PLAYER_INDEX];
	human *opponent = &data.humans[OPPONENT_INDEX];

	void *player_tool_globals = data.tool_globals[PLAYER_INDEX];
	void *opponent_tool_globals = data.tool_globals[OPPONENT_INDEX];

	tool *player_tool = &data.tools[PLAYER_INDEX];
	tool *opponent_tool = &data.tools[OPPONENT_INDEX];

	printf("You have %d health\n", player->health);
	printf("The opponent has %d health\n\n", opponent->health);

	typeof(on_tool_use) *use = player_tool->on_fns->use;
	if (use) {
		printf("You use your %s\n", player_tool->name);
		use(player_tool_globals);
		sleep(1);
	} else {
		printf("You don't know what to do with your %s\n", player_tool->name);
		sleep(1);
	}

	if (opponent->health <= 0) {
		printf("The opponent died!\n");
		sleep(1);
		data.state = STATE_PICKING_PLAYER;
		data.gold += opponent->kill_gold_value;
		player->health = player->max_health;
		return;
	}

	use = opponent_tool->on_fns->use;
	if (use) {
		printf("The opponent uses their %s\n", opponent_tool->name);
		use(opponent_tool_globals);
		sleep(1);
	} else {
		printf("The opponent doesn't know what to do with their %s\n", opponent_tool->name);
		sleep(1);
	}

	if (player->health <= 0) {
		printf("You died!\n");
		sleep(1);
		data.state = STATE_PICKING_PLAYER;
		player->health = player->max_health;
		return;
	}
}

static void discard_unread(void) {
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

static void print_opponent_humans(struct grug_file *files_defining_human) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		files_defining_human[i].define_fn();
		printf("%ld. %s, worth %d gold when killed\n", i + 1, human_definition.name, human_definition.kill_gold_value);
	}
	printf("\n");
}

static void pick_opponent(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *files_defining_human = get_type_files("human");

	print_opponent_humans(files_defining_human);

	printf("Type the number next to the human you want to fight:\n");

	size_t opponent_number;
	if (!read_size(&opponent_number)) {
		return;
	}

	if (opponent_number == 0) {
		fprintf(stderr, "The minimum number you can enter is 1\n");
		return;
	}
	if (opponent_number > data.type_files_size) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.type_files_size);
		return;
	}

	size_t opponent_index = opponent_number - 1;

	struct grug_file file = files_defining_human[opponent_index];

	file.define_fn();
	human human = human_definition;

	human.id = OPPONENT_INDEX;
	human.opponent_id = PLAYER_INDEX;

	human.max_health = human.health;

	data.humans[OPPONENT_INDEX] = human;
	data.human_dlls[OPPONENT_INDEX] = file.dll;

	free(data.human_globals[OPPONENT_INDEX]);
	data.human_globals[OPPONENT_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.human_globals[OPPONENT_INDEX], OPPONENT_INDEX);

	// Give the opponent a random tool
	struct grug_file *files_defining_tool = get_type_files("tool");
	size_t tool_index = rand() % data.type_files_size;

	file = files_defining_tool[tool_index];

	file.define_fn();
	tool tool = tool_definition;

	tool.on_fns = file.on_fns;

	tool.human_parent_id = OPPONENT_INDEX;

	data.tools[OPPONENT_INDEX] = tool;
	data.tool_dlls[OPPONENT_INDEX] = file.dll;

	free(data.tool_globals[OPPONENT_INDEX]);
	data.tool_globals[OPPONENT_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.tool_globals[OPPONENT_INDEX], OPPONENT_INDEX);

	data.state = STATE_FIGHTING;
}

static void print_tools(struct grug_file *files_defining_tool) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		files_defining_tool[i].define_fn();
		tool tool = tool_definition;
		printf("%ld. %s costs %d gold\n", i + 1, tool.name, tool.buy_gold_value);
	}
	printf("\n");
}

static void pick_tools(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *files_defining_tool = get_type_files("tool");

	print_tools(files_defining_tool);

	printf("Type the number next to the tool you want to buy%s:\n", data.player_has_tool ? " (type 0 to skip)" : "");

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
	if (tool_number > data.type_files_size) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.type_files_size);
		return;
	}

	size_t tool_index = tool_number - 1;

	struct grug_file file = files_defining_tool[tool_index];

	file.define_fn();
	tool tool = tool_definition;

	tool.on_fns = file.on_fns;

	if (tool.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to buy that tool\n");
		return;
	}

	data.gold -= tool.buy_gold_value;

	tool.human_parent_id = PLAYER_INDEX;

	data.tools[PLAYER_INDEX] = tool;
	data.tool_dlls[PLAYER_INDEX] = file.dll;

	free(data.tool_globals[PLAYER_INDEX]);
	data.tool_globals[PLAYER_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.tool_globals[PLAYER_INDEX], PLAYER_INDEX);

	data.player_has_tool = true;
}

static void print_playable_humans(struct grug_file *files_defining_human) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		files_defining_human[i].define_fn();
		human human = human_definition;
		printf("%ld. %s, costing %d gold\n", i + 1, human.name, human.buy_gold_value);
	}
	printf("\n");
}

static void pick_player(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *files_defining_human = get_type_files("human");

	print_playable_humans(files_defining_human);

	printf("Type the number next to the human you want to play as%s:\n", data.player_has_human ? " (type 0 to skip)" : "");

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
	if (player_number > data.type_files_size) {
		fprintf(stderr, "The maximum number you can enter is %ld\n", data.type_files_size);
		return;
	}

	size_t player_index = player_number - 1;

	struct grug_file file = files_defining_human[player_index];

	file.define_fn();
	human human = human_definition;

	if (human.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to pick that human\n");
		return;
	}

	data.gold -= human.buy_gold_value;

	human.id = PLAYER_INDEX;
	human.opponent_id = OPPONENT_INDEX;

	human.max_health = human.health;

	data.humans[PLAYER_INDEX] = human;
	data.human_dlls[PLAYER_INDEX] = file.dll;

	free(data.human_globals[PLAYER_INDEX]);
	data.human_globals[PLAYER_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.human_globals[PLAYER_INDEX], PLAYER_INDEX);

	data.player_has_human = true;

	data.state = STATE_PICKING_TOOLS;
}

static void update(void) {
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

static void reload_modified_entities(void) {
	for (size_t reload_index = 0; reload_index < grug_reloads_size; reload_index++) {
		struct grug_modified reload = grug_reloads[reload_index];

		for (size_t i = 0; i < 2; i++) {
			if (reload.old_dll == data.human_dlls[i]) {
				data.human_dlls[i] = reload.file->dll;

				free(data.human_globals[i]);
				data.human_globals[i] = malloc(reload.file->globals_size);
				reload.file->init_globals_fn(data.human_globals[i], i);
			}
		}
		for (size_t i = 0; i < 2; i++) {
			if (reload.old_dll == data.tool_dlls[i]) {
				data.tool_dlls[i] = reload.file->dll;

				free(data.tool_globals[i]);
				data.tool_globals[i] = malloc(reload.file->globals_size);
				reload.file->init_globals_fn(data.tool_globals[i], i);

				data.tools[i].on_fns = reload.file->on_fns;
			}
		}
	}
}

static void runtime_error_handler(char *reason, enum grug_runtime_error_type type, char *on_fn_name, char *on_fn_path) {
	(void)type;

	fprintf(stderr, "grug runtime error in %s(): %s, in %s\n", on_fn_name, reason, on_fn_path);
}

int main(void) {
	// Seed the random number generator with the number of seconds since 1970
	srand(time(NULL));

	grug_set_runtime_error_handler(runtime_error_handler);

	init_data();

	while (true) {
		if (grug_regenerate_modified_mods()) {
			if (grug_error.has_changed) {
				fprintf(stderr, "grug loading error: %s, in %s (detected in grug.c:%d)\n", grug_error.msg, grug_error.path, grug_error.grug_c_line_number);
			}

			sleep(1);

			continue;
		}

		reload_modified_entities();

		// Since this is a terminal game, there are no PNGs/MP3s/etc.
		// reload_modified_resources();

		update();

		printf("\n");

		sleep(1);
	}

	grug_free_mods();
	free_data();
}
