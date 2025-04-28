#include "data.h"
#include "game/tool.h"
#include "grug.h"
#include "macros.h"

#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

char game_function_error[MAX_GAME_FUNCTION_ERROR_CHARS];

#define SET_CALLED(property) { \
	if (set_ ## property ## _called) { \
		fprintf(stderr, "set_" #property "() was called twice by on_spawn()\n"); \
		return; \
	} \
	set_ ## property ## _called = true; \
	\
}

#define ASSERT_HAS_ON_SPAWN() { \
	if (!on_fns->spawn) { \
		fprintf(stderr, "%s is missing on_spawn()\n", entity); \
		return true; \
	} \
}

#define ASSERT_ON_SPAWN_PROPERTY_SET(property) { \
	if (!property ## _called) { \
		fprintf(stderr, "%s its on_spawn() did not call " #property "()\n", entity); \
		return true; \
	} \
}

void game_fn_print_string(char *msg) {
	printf("%s\n", msg);
}

static struct human human_on_spawn_data;
static bool set_human_name_called;
void game_fn_set_human_name(char *name) {
	SET_CALLED(human_name);
	human_on_spawn_data.name = name;
}
static bool set_human_health_called;
void game_fn_set_human_health(i32 health) {
	SET_CALLED(human_health);
	human_on_spawn_data.health = health;
}
static bool set_human_buy_gold_value_called;
void game_fn_set_human_buy_gold_value(i32 buy_gold_value) {
	SET_CALLED(human_buy_gold_value);
	human_on_spawn_data.buy_gold_value = buy_gold_value;
}
static bool set_human_kill_gold_value_called;
void game_fn_set_human_kill_gold_value(i32 kill_gold_value) {
	SET_CALLED(human_kill_gold_value);
	human_on_spawn_data.kill_gold_value = kill_gold_value;
}

static struct tool tool_on_spawn_data;
static bool set_tool_name_called;
void game_fn_set_tool_name(char *name) {
	SET_CALLED(tool_name);
	tool_on_spawn_data.name = name;
}
static bool set_tool_buy_gold_value_called;
void game_fn_set_tool_buy_gold_value(i32 buy_gold_value) {
	SET_CALLED(tool_buy_gold_value);
	tool_on_spawn_data.buy_gold_value = buy_gold_value;
}

static void push_file_containing_fn(struct grug_file file) {
	if (data.type_files_size + 1 > MAX_TYPE_FILES) {
		fprintf(stderr, "There are more than %d files containing the requested type, exceeding MAX_TYPE_FILES", MAX_TYPE_FILES);
		exit(EXIT_FAILURE);
	}
	data.type_files[data.type_files_size++] = file;
}

static void get_type_files_impl(struct grug_mod_dir dir, char *entity_type) {
	for (size_t i = 0; i < dir.dirs_size; i++) {
		get_type_files_impl(dir.dirs[i], entity_type);
	}

	for (size_t i = 0; i < dir.files_size; i++) {
		if (strcmp(dir.files[i].entity_type, entity_type) == 0) {
			push_file_containing_fn(dir.files[i]);
		}
	}
}

static struct grug_file *get_type_files(char *entity_type) {
	data.type_files_size = 0;
	get_type_files_impl(grug_mods, entity_type);
	return data.type_files;
}

static void call_human_on_despawn(human_on_fns *on_fns, void *globals) {
	if (on_fns->despawn) {
		on_fns->despawn(globals);
	}
}

static void call_tool_on_despawn(tool_on_fns *on_fns, void *globals) {
	if (on_fns->despawn) {
		on_fns->despawn(globals);
	}
}

static void fight(void) {
	human *player = &data.humans[PLAYER_INDEX];
	human *opponent = &data.humans[OPPONENT_INDEX];

	tool *player_tool = &data.tools[PLAYER_INDEX];
	tool *opponent_tool = &data.tools[OPPONENT_INDEX];

	void *opponent_human_globals = data.human_globals[OPPONENT_INDEX];

	void *player_tool_globals = data.tool_globals[PLAYER_INDEX];
	void *opponent_tool_globals = data.tool_globals[OPPONENT_INDEX];

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
		call_human_on_despawn(opponent->on_fns, opponent_human_globals);
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

	if (buffer[0] == 'f' && buffer[1] == '\n') {
		grug_toggle_on_fns_mode();
		printf("Toggled grug to %s mode\n", grug_are_on_fns_in_safe_mode() ? "safe" : "fast");
		return false;
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

static bool call_human_on_spawn(char *entity, human_on_fns *on_fns, void *globals) {
	ASSERT_HAS_ON_SPAWN();

	set_human_name_called = false;
	set_human_health_called = false;
	set_human_buy_gold_value_called = false;
	set_human_kill_gold_value_called = false;

	on_fns->spawn(globals);

	ASSERT_ON_SPAWN_PROPERTY_SET(set_human_name);
	ASSERT_ON_SPAWN_PROPERTY_SET(set_human_health);
	ASSERT_ON_SPAWN_PROPERTY_SET(set_human_buy_gold_value);
	ASSERT_ON_SPAWN_PROPERTY_SET(set_human_kill_gold_value);

	return false;
}

static bool call_tool_on_spawn(char *entity, tool_on_fns *on_fns, void *globals) {
	ASSERT_HAS_ON_SPAWN();

	set_tool_name_called = false;
	set_tool_buy_gold_value_called = false;

	on_fns->spawn(globals);

	ASSERT_ON_SPAWN_PROPERTY_SET(set_tool_name);
	ASSERT_ON_SPAWN_PROPERTY_SET(set_tool_buy_gold_value);

	return false;
}

static bool print_opponent_humans(struct grug_file *human_files) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		struct grug_file file = human_files[i];

		void *globals = malloc(file.globals_size);
		file.init_globals_fn(globals, 0);

		if (call_human_on_spawn(file.entity, file.on_fns, globals)) {
			free(globals);
			return true;
		}

		human human = human_on_spawn_data;

		printf("%ld. %s, worth %d gold when killed\n", i + 1, human.name, human.kill_gold_value);

		call_human_on_despawn(file.on_fns, globals);

		free(globals);
	}
	printf("\n");
	return false;
}

static void pick_opponent(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *human_files = get_type_files("human");

	if (print_opponent_humans(human_files)) {
		return;
	}

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

	struct grug_file file = human_files[opponent_index];

	free(data.human_globals[OPPONENT_INDEX]);
	data.human_globals[OPPONENT_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.human_globals[OPPONENT_INDEX], OPPONENT_INDEX);

	if (call_human_on_spawn(file.entity, file.on_fns, data.human_globals[OPPONENT_INDEX])) {
		return;
	}

	human human = human_on_spawn_data;

	human.on_fns = file.on_fns;

	human.id = OPPONENT_INDEX;
	human.opponent_id = PLAYER_INDEX;

	human.max_health = human.health;

	data.humans[OPPONENT_INDEX] = human;
	data.human_dlls[OPPONENT_INDEX] = file.dll;

	// Give the opponent a random tool
	struct grug_file *tool_files = get_type_files("tool");
	size_t tool_index = rand() % data.type_files_size;

	file = tool_files[tool_index];

	free(data.tool_globals[OPPONENT_INDEX]);
	data.tool_globals[OPPONENT_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.tool_globals[OPPONENT_INDEX], OPPONENT_INDEX);

	if (call_tool_on_spawn(file.entity, file.on_fns, data.tool_globals[OPPONENT_INDEX])) {
		return;
	}

	tool tool = tool_on_spawn_data;

	tool.on_fns = file.on_fns;

	tool.human_parent_id = OPPONENT_INDEX;

	data.tools[OPPONENT_INDEX] = tool;
	data.tool_dlls[OPPONENT_INDEX] = file.dll;

	data.state = STATE_FIGHTING;
}

static bool print_tools(struct grug_file *tool_files) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		struct grug_file file = tool_files[i];

		void *globals = malloc(file.globals_size);
		file.init_globals_fn(globals, 0);

		if (call_tool_on_spawn(file.entity, file.on_fns, globals)) {
			free(globals);
			return true;
		}

		tool tool = tool_on_spawn_data;

		printf("%ld. %s, costing %d gold\n", i + 1, tool.name, tool.buy_gold_value);

		call_tool_on_despawn(file.on_fns, globals);

		free(globals);
	}
	printf("\n");
	return false;
}

static void pick_tools(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *tool_files = get_type_files("tool");

	if (print_tools(tool_files)) {
		return;
	}

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

	if (data.tools[PLAYER_INDEX].on_fns) {
		call_tool_on_despawn(data.tools[PLAYER_INDEX].on_fns, data.tool_globals[PLAYER_INDEX]);
	}

	size_t tool_index = tool_number - 1;

	struct grug_file file = tool_files[tool_index];

	free(data.tool_globals[PLAYER_INDEX]);
	data.tool_globals[PLAYER_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.tool_globals[PLAYER_INDEX], PLAYER_INDEX);

	if (call_tool_on_spawn(file.entity, file.on_fns, data.tool_globals[PLAYER_INDEX])) {
		return;
	}

	tool tool = tool_on_spawn_data;

	if (tool.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to buy that tool\n");
		return;
	}
	data.gold -= tool.buy_gold_value;

	tool.on_fns = file.on_fns;

	tool.human_parent_id = PLAYER_INDEX;

	data.tools[PLAYER_INDEX] = tool;
	data.tool_dlls[PLAYER_INDEX] = file.dll;

	data.player_has_tool = true;

	data.state = STATE_PICKING_OPPONENT;
}

static bool print_playable_humans(struct grug_file *human_files) {
	for (size_t i = 0; i < data.type_files_size; i++) {
		struct grug_file file = human_files[i];

		void *globals = malloc(file.globals_size);
		file.init_globals_fn(globals, 0);

		if (call_human_on_spawn(file.entity, file.on_fns, globals)) {
			free(globals);
			return true;
		}

		human human = human_on_spawn_data;

		printf("%ld. %s, costing %d gold\n", i + 1, human.name, human.buy_gold_value);

		call_human_on_despawn(file.on_fns, globals);

		free(globals);
	}
	printf("\n");
	return false;
}

static void pick_player(void) {
	printf("You have %d gold\n\n", data.gold);

	struct grug_file *human_files = get_type_files("human");

	if (print_playable_humans(human_files)) {
		return;
	}

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

	if (data.humans[PLAYER_INDEX].on_fns) {
		call_human_on_despawn(data.humans[PLAYER_INDEX].on_fns, data.human_globals[PLAYER_INDEX]);
	}

	size_t player_index = player_number - 1;

	struct grug_file file = human_files[player_index];

	free(data.human_globals[PLAYER_INDEX]);
	data.human_globals[PLAYER_INDEX] = malloc(file.globals_size);
	file.init_globals_fn(data.human_globals[PLAYER_INDEX], PLAYER_INDEX);

	if (call_human_on_spawn(file.entity, file.on_fns, data.human_globals[PLAYER_INDEX])) {
		return;
	}

	human human = human_on_spawn_data;

	if (human.buy_gold_value > data.gold) {
		fprintf(stderr, "You don't have enough gold to pick that human\n");
		return;
	}
	data.gold -= human.buy_gold_value;

	human.on_fns = file.on_fns;

	human.id = PLAYER_INDEX;
	human.opponent_id = OPPONENT_INDEX;

	human.max_health = human.health;

	data.humans[PLAYER_INDEX] = human;
	data.human_dlls[PLAYER_INDEX] = file.dll;

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
				data.human_dlls[i] = reload.file.dll;

				free(data.human_globals[i]);
				data.human_globals[i] = malloc(reload.file.globals_size);
				reload.file.init_globals_fn(data.human_globals[i], i);

				data.humans[i].on_fns = reload.file.on_fns;
			}
		}

		for (size_t i = 0; i < 2; i++) {
			if (reload.old_dll == data.tool_dlls[i]) {
				data.tool_dlls[i] = reload.file.dll;

				free(data.tool_globals[i]);
				data.tool_globals[i] = malloc(reload.file.globals_size);
				reload.file.init_globals_fn(data.tool_globals[i], i);

				data.tools[i].on_fns = reload.file.on_fns;
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

	if (grug_init(runtime_error_handler, "mod_api.json", "mods", 10)) {
		fprintf(stderr, "grug_init() error: %s (detected by grug.c:%d)\n", grug_error.msg, grug_error.grug_c_line_number);
		return EXIT_FAILURE;
	}

	init_data();

	while (true) {
		if (grug_regenerate_modified_mods()) {
			if (grug_error.has_changed) {
				if (grug_loading_error_in_grug_file) {
					fprintf(stderr, "grug loading error: %s, in %s (detected by grug.c:%d)\n", grug_error.msg, grug_error.path, grug_error.grug_c_line_number);
				} else {
					fprintf(stderr, "grug loading error: %s (detected by grug.c:%d)\n", grug_error.msg, grug_error.grug_c_line_number);
				}
			}

			sleep(1);

			continue;
		}

		reload_modified_entities();

		// Since this is a simple terminal game, there are no PNGs/MP3s/etc.
		// reload_modified_resources();

		update();

		printf("\n");

		sleep(1);
	}

	grug_free_mods();
	free_data();
}
