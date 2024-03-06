#include "data.h"
#include "grug.h"

#include <stdio.h>
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

int main() {
	init_data();

	printf("Hello, grug!\n");

	while (true) {
		grug_reload_modified_mods("mods");

		// update();

		printf("\n");

		sleep(1);
	}
	
	free_data();
}
