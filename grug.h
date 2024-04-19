#pragma once

#include <stdbool.h>
#include <stddef.h>
#include <typedefs.h>

typedef void (*grug_error_handler_fn)(char *error_msg, char *filename, int line_number);
extern grug_error_handler_fn grug_error_handler;

typedef void (*init_globals_struct_fn_type)(void *globals_struct);

typedef struct grug_file grug_file;
typedef struct mod_directory mod_directory;

struct grug_file {
	char *name;
	void *dll;
};

struct mod_directory {
	char *name;

	mod_directory *dirs;
	size_t dirs_size;
	size_t dirs_capacity;

	grug_file *files;
	size_t files_size;
	size_t files_capacity;
};

extern mod_directory mods;

extern void *old_dll;
extern void *new_dll;
extern size_t globals_struct_size;
extern init_globals_struct_fn_type init_globals_struct_fn;

void grug_free_mods(mod_directory dir);
bool grug_reload_modified_mods();
void grug_print_mods(mod_directory mods);
void *grug_get_fn_address(void *dll, char *fn_name);
