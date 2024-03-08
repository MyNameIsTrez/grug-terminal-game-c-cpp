#pragma once

struct mod_directory {
	char *name;
	struct mod_files *files;
	struct mod_directory *dirs;
};

struct mod_directory grug_reload_modified_mods(char *mods_dir_path, char *dll_dir_path);
