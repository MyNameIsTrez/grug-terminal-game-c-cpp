#include "grug.h"

#include "libtcc.h" // TODO: Get rid of this eventually

#include <assert.h>
#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

// "The problem is that you can't meaningfully define a constant like this
// in a header file. The maximum path size is actually to be something
// like a filesystem limitation, or at the very least a kernel parameter.
// This means that it's a dynamic value, not something preordained."
// https://eklitzke.org/path-max-is-tricky
#define STUPID_MAX_PATH 4096

static char *read_file(char *path) {
	FILE *f = fopen(path, "rb");
	if (!f) {
        perror("fopen()");
        exit(EXIT_FAILURE);
	}

	if (fseek(f, 0, SEEK_END)) {
        perror("fseek()");
        exit(EXIT_FAILURE);
	}

	long count = ftell(f);
	if (count == -1) {
        perror("ftell()");
        exit(EXIT_FAILURE);
	}

	rewind(f);

	char *text = malloc(count + 1);
	if (!text) {
		perror("malloc()");
        exit(EXIT_FAILURE);
	}

	ssize_t bytes_read = fread(text, 1, count, f);
	if (bytes_read != count) {
        perror("fread()");
        exit(EXIT_FAILURE);
	}

	text[count] = '\0';

	return text;
}

static void handle_error(void *opaque, const char *msg) {
    fprintf(opaque, "%s\n", msg);
}

static void reload_grug_file(char *grug_file_path, char *dll_path) {
	printf("Reloading grug file '%s'\n", grug_file_path);

    TCCState *s = tcc_new();
    if (!s) {
        fprintf(stderr, "tcc_new() error\n");
        exit(EXIT_FAILURE);
    }

    tcc_set_error_func(s, stderr, handle_error);

	// TODO: This will probably be needed when files are moved around?
    /* if tcclib.h and libtcc1.a are not installed, where can we find them */
    // tcc_set_lib_path(s, a+2);
    // tcc_add_include_path(s, a+2);
    // tcc_add_library_path(s, a+2);
	
    if (tcc_set_output_type(s, TCC_OUTPUT_DLL)) {
        fprintf(stderr, "tcc_set_output_type() error\n");
        exit(EXIT_FAILURE);
    }

	char *c_text = read_file(grug_file_path);
    if (tcc_compile_string(s, c_text) == -1) {
        fprintf(stderr, "tcc_compile_string() error\n");
        exit(EXIT_FAILURE);
    }

    if (tcc_output_file(s, dll_path)) {
        fprintf(stderr, "tcc_output_file() error\n");
        exit(EXIT_FAILURE);
    }

    tcc_delete(s);
	free(c_text);
	errno = 0;
}

static void try_create_parent_dirs(char *file_path) {
	// printf("file_path: %s\n", file_path);

	char parent_dir_path[STUPID_MAX_PATH];
	size_t i = 0;

	errno = 0;
	while (*file_path) {
		parent_dir_path[i] = *file_path;
		parent_dir_path[i + 1] = '\0';

		// printf("parent_dir_path: '%s'\n", parent_dir_path);

		if (*file_path == '/' || *file_path == '\\') {
			if (mkdir(parent_dir_path, 0777) && errno != EEXIST) {
				perror("mkdir");
				exit(EXIT_FAILURE);
			}
		}

		file_path++;
		i++;
	}
}

static char *get_file_extension(char *filename) {
	char *ext = strrchr(filename, '.');
	if (ext) {
		return ext;
	}
	return "";
}

static void fill_dll_path(char *dll_path, char *grug_file_path) {
	dll_path[0] = '\0';
	strncat(dll_path, grug_file_path, STUPID_MAX_PATH - 1);
	char *ext = get_file_extension(dll_path);
	assert(*ext);
	ext[1] = '\0';
	strncat(ext + 1, "so", STUPID_MAX_PATH - 1 - strlen(dll_path));
}

void grug_reload_modified_mods(char *mods_dir_path, char *dll_dir_path) {
	// printf("opendir(\"%s\")\n", mods_dir_path);
	DIR *dirp = opendir(mods_dir_path);
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

		char entry_path[STUPID_MAX_PATH];
		snprintf(entry_path, sizeof(entry_path), "%s/%s", mods_dir_path, dp->d_name);
		// printf("entry_path is %s\n", entry_path);

		struct stat buf;
		if (stat(entry_path, &buf) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		char dll_entry_path[STUPID_MAX_PATH];
		snprintf(dll_entry_path, sizeof(dll_entry_path), "%s/%s", dll_dir_path, dp->d_name);

		if (S_ISDIR(buf.st_mode)) {
			grug_reload_modified_mods(entry_path, dll_entry_path);
		} else if (S_ISREG(buf.st_mode) && strcmp(get_file_extension(dp->d_name), ".c") == 0) {
			char dll_path[STUPID_MAX_PATH];
			fill_dll_path(dll_path, dll_entry_path);
			// printf("dll path: %s\n", dll_path);

			errno = 0;
			if (access(dll_path, F_OK) && errno == ENOENT) {
				try_create_parent_dirs(dll_path);
				errno = 0;
			}
			if (errno != 0 && errno != ENOENT) {
				fprintf(stderr, "errno was not 0 after access()\n");
				exit(EXIT_FAILURE);
			}

			reload_grug_file(entry_path, dll_path);
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);
}
