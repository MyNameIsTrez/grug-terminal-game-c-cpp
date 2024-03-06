#include "grug.h"

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

// static void load_mod(char *mod_name) {
// 	printf("Loading mod '%s'\n", mod_name);

// 	// TODO: DLL loading logic
// }

static char *get_file_extension(char *filename) {
	char *ext = strrchr(filename, '.');
	if (ext) {
		return ext;
	}
	return "";
}

void grug_reload_modified_mods(char *dir_path) {
	// printf("opendir(\"%s\")\n", dir_path);
	DIR *dirp = opendir(dir_path);
	if (!dirp) {
		perror("opendir");
		exit(EXIT_FAILURE);
	}

	errno = 0;
	struct dirent *dp;
	while ((dp = readdir(dirp))) {
		if (strcmp(dp->d_name, ".") == 0 || strcmp(dp->d_name, "..") == 0) {
			continue;
		}

		char entry_path[1024];
		sprintf(entry_path, "%s/%s", dir_path, dp->d_name);
		// printf("entry_path is %s\n", entry_path);

		struct stat buf;
		if (stat(entry_path, &buf) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		if (S_ISDIR(buf.st_mode)) {
			grug_reload_modified_mods(entry_path);
		} else if (S_ISREG(buf.st_mode) && strcmp(get_file_extension(dp->d_name), ".c") == 0) {
			printf("%s detected\n", dp->d_name);
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);
}
