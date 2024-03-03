#include "data.h"

#include <dirent.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct data data;

static long read_long() {
	char buffer[42];
	if (!fgets(buffer, sizeof(buffer), stdin)) {
		perror("fgets");
		exit(EXIT_FAILURE);
	}

	char *endptr;
	errno = 0;
	long l = strtol(buffer, &endptr, 10);
	if (errno != 0) {
		perror("strtol");
		exit(EXIT_FAILURE);
	} else if (buffer == endptr) {
		fprintf(stderr, "No number was provided\n");
		exit(EXIT_FAILURE);
	} else if (*endptr != '\n' && *endptr != '\0') {
		fprintf(stderr, "There were further characters after the number\n");
		fprintf(stderr, "'%s'\n", endptr);
		exit(EXIT_FAILURE);
	}

	return l;
}

static void pick_humans() {
	printf("In pick_humans()\n");

	printf("Type the index of the human that should fight in team 1:\n");
	size_t human1_index = read_long();

	printf("Type the index of the human that should fight in team 2:\n");
	size_t human2_index = read_long();
	
	printf("human1_index: %lld\n", human1_index);
	printf("human2_index: %lld\n", human2_index);
}

static void fight() {
	printf("In fight()\n");
}

static void update() {
	printf("In update()\n");

	if (data.fighting) {
		fight();
	} else {
		pick_humans();
	}
}

static void load_mod(char *mod_name) {
	printf("Loading mod '%s'\n", mod_name);

	// TODO: DLL loading logic
}

static void reload_modified_mods() {
	DIR *dirp = opendir("mods");
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
		
		char path[PATH_MAX] = "mods/";
		memcpy(path + sizeof("mods/") - 1, dp->d_name, strlen(dp->d_name) + 1);
		// printf("%s\n", path);

		struct stat buf;
		if (stat(path, &buf) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		if (S_ISDIR(buf.st_mode)) {
			load_mod(dp->d_name);
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);
}

int main() {
	init_data();

	printf("Hello, grug!\n");

	while (true) {
		reload_modified_mods();

		update();

		sleep(1);
	}
	
	free_data();
}
