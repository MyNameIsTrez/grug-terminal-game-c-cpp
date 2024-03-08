#pragma once

#include <stddef.h>

struct grug_file {
	char *name;
	void *dll;
};

struct mod_directory {
	char *name;

	struct mod_directory *dirs;
	size_t dirs_size;
	size_t dirs_capacity;

	struct grug_file *files;
	size_t files_size;
	size_t files_capacity;
};

void grug_free_mods(struct mod_directory dir);
struct mod_directory grug_reload_modified_mods(char *mods_dir_path, char *mods_dir_name, char *dll_dir_path);
void grug_print_mods(struct mod_directory mods);
void *grug_get_fn_address(void *dll, char *fn_name);
