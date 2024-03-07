#include "data.h"
#include "grug.h"

// TODO: REMOVE
#include "game/tool.h"

#include <dlfcn.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

struct data data;

// static long read_long() {
// 	char buffer[42];
// 	if (!fgets(buffer, sizeof(buffer), stdin)) {
// 		perror("fgets");
// 		exit(EXIT_FAILURE);
// 	}

// 	char *endptr;
// 	errno = 0;
// 	long l = strtol(buffer, &endptr, 10);
// 	if (errno != 0) {
// 		perror("strtol");
// 		exit(EXIT_FAILURE);
// 	} else if (buffer == endptr) {
// 		fprintf(stderr, "No number was provided\n");
// 		exit(EXIT_FAILURE);
// 	} else if (*endptr != '\n' && *endptr != '\0') {
// 		fprintf(stderr, "There were further characters after the number\n");
// 		fprintf(stderr, "'%s'\n", endptr);
// 		exit(EXIT_FAILURE);
// 	}

// 	return l;
// }

// static void pick_humans() {
// 	printf("In pick_humans()\n");

// 	printf("Type the index of the human that should fight in team 1:\n");
// 	size_t human1_index = read_long();

// 	printf("Type the index of the human that should fight in team 2:\n");
// 	size_t human2_index = read_long();
	
// 	printf("human1_index: %ld\n", human1_index);
// 	printf("human2_index: %ld\n", human2_index);
// }

// static void fight() {
// 	printf("In fight()\n");
// }

// static void update() {
// 	printf("In update()\n");

// 	if (data.fighting) {
// 		fight();
// 	} else {
// 		pick_humans();
// 	}
// }

// TODO: REMOVE
typedef struct tool (*define_tool_fn)();

// TODO: REMOVE
static void print_dlerror(char *function_name) {
	char *err = dlerror();
	if (!err) {
		printf("dlerror was asked to find an error string, but it couldn't find one\n");
		exit(EXIT_FAILURE);
	}
	printf("dlerror in %s():\n%s\n", function_name, err);
	exit(EXIT_FAILURE);
}

// TODO: REMOVE
/* this function is called by the generated code */
int add(int a, int b)
{
    return a + b;
}

int main() {
	init_data();

	printf("Hello, grug!\n");

	while (true) {
		grug_reload_modified_mods("mods", "dlls");

		// update();

		void *dll = dlopen("./dlls/magic/potions/health.so", RTLD_NOW);
		if (!dll) {
			print_dlerror("load_dynamic_library");
		}

		// This suppresses the warning "ISO C forbids conversion of object pointer to function pointer type"
		#pragma GCC diagnostic push
		#pragma GCC diagnostic ignored "-Wpedantic"
		define_tool_fn define_tool = dlsym(dll, "define_tool");
		#pragma GCC diagnostic pop
		if (!define_tool) {
			print_dlerror("load_dynamic_function");
		}

		printf("The Health Potion's monetary_value is %d\n", define_tool().monetary_value);

		if (dlclose(dll)) {
			print_dlerror("free_dynamic_library");
		}

		sleep(1);
	}
	
	free_data();
}
