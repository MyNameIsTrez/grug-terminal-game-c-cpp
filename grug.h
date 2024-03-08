#pragma once

#include <stddef.h>

struct mod_file {
	char *name;
	void *dll;
	struct tool (*define_tool_fn)();
};

struct mod_directory {
	char *name;

	struct mod_directory *dirs;
	size_t dirs_size;
	size_t dirs_capacity;

	struct mod_file *files;
	size_t files_size;
	size_t files_capacity;
};

void grug_free_mods(struct mod_directory dir);
struct mod_directory grug_reload_modified_mods(char *mods_dir_path, char *mods_dir_name, char *dll_dir_path);
void grug_print_mods(struct mod_directory mods);
