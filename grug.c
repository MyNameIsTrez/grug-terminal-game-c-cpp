#include "grug.h"

#include "libtcc.h" // TODO: Get rid of this eventually

#include <assert.h>
#include <dirent.h>
#include <dlfcn.h>
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

// TODO: USE
// static char **get_define_fn_names() {
// 	return (char *[]){
// 		"define_tool",
// 		NULL
// 	};
// }

// TODO: USE
// static char **get_on_fn_names() {
// 	return (char *[]){
// 		"on_tool_use",
// 		NULL
// 	};
// }

typedef struct token token;
typedef struct tokens tokens;
typedef struct call_expr call_expr;
typedef struct unary_expr unary_expr;
typedef struct binary_expr binary_expr;
typedef struct literal literal;
typedef struct expr expr;
typedef struct return_statement return_statement;
typedef struct if_statement if_statement;
typedef struct statement statement;
typedef struct node node;
typedef struct nodes nodes;
typedef struct argument argument;
typedef struct arguments arguments;
typedef struct fn fn;
typedef struct fns fns;

struct token {
	enum {
		TEXT_TOKEN,
		OPEN_PARENTHESIS_TOKEN,
		CLOSE_PARENTHESIS_TOKEN,
		OPEN_BRACE_TOKEN,
		CLOSE_BRACE_TOKEN,
		STRING_TOKEN,
		COMMA_TOKEN,
		FIELD_NAME_TOKEN,
		COLON_TOKEN,
		PLUS_TOKEN,
		MINUS_TOKEN,
		ASSIGNMENT_EQUALS_TOKEN,
		EQUALITY_EQUALS_TOKEN,
		NUMBER_TOKEN,
		IF_TOKEN,
		LOOP_TOKEN,
		BREAK_TOKEN,
		CONTINUE_TOKEN,
		RETURN_TOKEN,
	} type;
	char *str;
};

struct tokens {
	token *tokens;
	size_t size;
	size_t capacity;
};

struct call_expr {
	char *fn_name;
	expr *arguments;
	size_t size;
	size_t capacity;
};

struct unary_expr {
	enum {
		MINUS_UNARY_EXPR,
	} operator;
	expr *expr;
};

struct binary_expr {
	enum {
		ADDITION,
		SUBTRACTION,
		MULTIPLICATION,
		DIVISION,
	} operator;
	expr *left;
	expr *right;
};

struct literal {
	char *str;
};

struct expr {
	enum {
		LITERAL,
		UNARY_EXPR,
		BINARY_EXPR,
		CALL_EXPR,
	} type;
	union {
		literal literal;
		unary_expr unary_expr;
		binary_expr binary_expr;
		call_expr call_expr;
	};
};

struct return_statement {
	expr value;
};

struct if_statement {
	expr condition;
	nodes *body;
	nodes *else_body;
};

struct statement {
	char *variable_name;
	char *type;
	expr value;
};

struct node {
	enum {
		STATEMENT,
		IF,
		LOOP,
		BREAK,
		CONTINUE,
		RETURN,
	} type;
	union {
		statement statement;
		if_statement if_statement;
		return_statement return_statement;
	};
};

struct nodes {
	node *nodes;
	size_t size;
	size_t capacity;
};

struct argument {
	char *type;
	char *name;
};

struct arguments {
	argument *arguments;
	size_t size;
	size_t capacity;
};

struct fn {
	char *fn_name;
	arguments arguments;
	char *return_type;
	nodes body;
};

struct fns {
	fn *fns;
	size_t size;
	size_t capacity;
};

static char *serialize_to_c(fns fns) {
	char *c_text;

	(void)fns;
	c_text = "";

	return c_text;
}

static fns parse(tokens tokens) {
	fns fns;

	(void)tokens;
	fns.fns = NULL;

	return fns;
}

static tokens tokenize(char *grug_text) {
	tokens tokens;

	(void)grug_text;
	tokens.tokens = NULL;

	return tokens;
}

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

static void regenerate_dll(char *grug_file_path, char *dll_path) {
	printf("Regenerating %s\n", dll_path);

    TCCState *s = tcc_new();
    if (!s) {
        fprintf(stderr, "tcc_new() error\n");
        exit(EXIT_FAILURE);
    }

    tcc_set_error_func(s, stderr, handle_error);

    tcc_add_include_path(s, ".");
	
    if (tcc_set_output_type(s, TCC_OUTPUT_DLL)) {
        fprintf(stderr, "tcc_set_output_type() error\n");
        exit(EXIT_FAILURE);
    }

	char *grug_text = read_file(grug_file_path);

	tokens tokens = tokenize(grug_text);

	fns fns = parse(tokens);

	char *c_text = serialize_to_c(fns);
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

static void use_dll_extension(char *dll_path, char *grug_file_path) {
	dll_path[0] = '\0';
	strncat(dll_path, grug_file_path, STUPID_MAX_PATH - 1);
	char *ext = get_file_extension(dll_path);
	assert(*ext);
	ext[1] = '\0';
	strncat(ext + 1, "so", STUPID_MAX_PATH - 1 - strlen(dll_path));
}

static void print_dlerror(char *function_name) {
	char *err = dlerror();
	if (!err) {
		fprintf(stderr, "dlerror was asked to find an error string, but it couldn't find one\n");
		exit(EXIT_FAILURE);
	}
	fprintf(stderr, "%s: %s\n", function_name, err);
	exit(EXIT_FAILURE);
}

// TODO: Don't free and realloc everything every time our function gets called
// TODO: Also, stop presuming the game developer will always call this before grug_reload_modified_mods()
void grug_free_mods(mod_directory dir) {
	free(dir.name);

	for (size_t i = 0; i < dir.dirs_size; i++) {
		grug_free_mods(dir.dirs[i]);
	}
	free(dir.dirs);

	for (size_t i = 0; i < dir.files_size; i++) {
		free(dir.files[i].name);

		if (dir.files[i].dll && dlclose(dir.files[i].dll)) {
			print_dlerror("dlclose");
		}
	}
	free(dir.files);
}

mod_directory grug_reload_modified_mods(char *mods_dir_path, char *mods_dir_name, char *dll_dir_path) {
	mod_directory mod_dir = {0};

	mod_dir.name = strdup(mods_dir_name);
	if (!mod_dir.name) {
		perror("strdup");
		exit(EXIT_FAILURE);
	}

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

		struct stat entry_stat;
		if (stat(entry_path, &entry_stat) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		char dll_entry_path[STUPID_MAX_PATH];
		snprintf(dll_entry_path, sizeof(dll_entry_path), "%s/%s", dll_dir_path, dp->d_name);

		if (S_ISDIR(entry_stat.st_mode)) {
			mod_directory mod_subdir = grug_reload_modified_mods(entry_path, dp->d_name, dll_entry_path);
			
			// Make sure there's enough room for pushing mod_subdir
			if (mod_dir.dirs_size + 1 > mod_dir.dirs_capacity) {
				mod_dir.dirs_capacity = mod_dir.dirs_capacity == 0 ? 1 : mod_dir.dirs_capacity * 2;
				mod_dir.dirs = realloc(mod_dir.dirs, mod_dir.dirs_capacity * sizeof(mod_directory));
				if (!mod_dir.dirs) {
					perror("realloc");
					exit(EXIT_FAILURE);
				}
			}

			mod_dir.dirs[mod_dir.dirs_size++] = mod_subdir;
		} else if (S_ISREG(entry_stat.st_mode) && strcmp(get_file_extension(dp->d_name), ".c") == 0) {
			char dll_path[STUPID_MAX_PATH];
			use_dll_extension(dll_path, dll_entry_path);
			// printf("dll path: %s\n", dll_path);

			// Regenerate the dll if it doesn't exist/is outdated
			struct stat dll_stat;
			if (stat(dll_path, &dll_stat) == -1 || entry_stat.st_mtime > dll_stat.st_mtime) {
				// If the dll doesn't exist, try to create the parent directories
				errno = 0;
				if (access(dll_path, F_OK) && errno == ENOENT) {
					try_create_parent_dirs(dll_path);
					errno = 0;
				}
				if (errno != 0 && errno != ENOENT) {
					fprintf(stderr, "errno was not 0 after access()\n");
					exit(EXIT_FAILURE);
				}

				regenerate_dll(entry_path, dll_path);
			}

			grug_file file = {0};

			file.name = strdup(dp->d_name);
			if (!file.name) {
				perror("strdup");
				exit(EXIT_FAILURE);
			}

			file.dll = dlopen(dll_path, RTLD_NOW);
			if (!file.dll) {
				print_dlerror("dlopen");
			}

			// Make sure there's enough room for pushing file
			if (mod_dir.files_size + 1 > mod_dir.files_capacity) {
				mod_dir.files_capacity = mod_dir.files_capacity == 0 ? 1 : mod_dir.files_capacity * 2;
				mod_dir.files = realloc(mod_dir.files, mod_dir.files_capacity * sizeof(grug_file));
				if (!mod_dir.files) {
					perror("realloc");
					exit(EXIT_FAILURE);
				}
			}

			mod_dir.files[mod_dir.files_size++] = file;
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);

	return mod_dir;
}

void grug_print_mods(mod_directory dir) {
	static int depth;

	printf("%*s%s/\n", depth * 2, "", dir.name);

	depth++;
	for (size_t i = 0; i < dir.dirs_size; i++) {
		grug_print_mods(dir.dirs[i]);
	}
	for (size_t i = 0; i < dir.files_size; i++) {
		printf("%*s%s\n", depth * 2, "", dir.files[i].name);
	}
	depth--;
}
