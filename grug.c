#include "grug.h"

#include "libtcc.h" // TODO: Get rid of this eventually

#include <dirent.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

static void handle_error(void *opaque, const char *msg)
{
    fprintf(opaque, "%s\n", msg);
}

// TODO: REMOVE
char my_program[] =
"#include <tcclib.h>\n" /* include the "Simple libc header for TCC" */
"extern int add(int a, int b);\n"
"\n"
"int foo(int n)\n"
"{\n"
"    printf(\"add(%d, %d) = %d\\n\", n, 2 * n, add(n, 2 * n));\n"
"    return 0;\n"
"}\n";

static void reload_grug_file(char *grug_file_path) {
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
    
    if (tcc_compile_string(s, my_program) == -1) {
        fprintf(stderr, "tcc_compile_string() error\n");
        exit(EXIT_FAILURE);
    }

    if (tcc_output_file(s, "foo.so")) {
        fprintf(stderr, "tcc_output_file() error\n");
        exit(EXIT_FAILURE);
    }

    tcc_delete(s);

	errno = 0;
}

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
		snprintf(entry_path, sizeof(entry_path), "%s/%s", dir_path, dp->d_name);
		// printf("entry_path is %s\n", entry_path);

		struct stat buf;
		if (stat(entry_path, &buf) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		if (S_ISDIR(buf.st_mode)) {
			grug_reload_modified_mods(entry_path);
		} else if (S_ISREG(buf.st_mode) && strcmp(get_file_extension(dp->d_name), ".c") == 0) {
			reload_grug_file(entry_path);
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);
}
