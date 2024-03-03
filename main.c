#include <dirent.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

int main() {
	printf("Hello, grug!\n");

	DIR *dirp = opendir("mods");
	if (!dirp) {
		perror("opendir");
		exit(EXIT_FAILURE);
	}

	errno = 0;
	struct dirent *dp;
	while ((dp = readdir(dirp))) {
		char path[PATH_MAX] = "mods/";
		memcpy(path + sizeof("mods/") - 1, dp->d_name, strlen(dp->d_name) + 1);
		printf("%s\n", path);

		struct stat buf;
		if (stat(path, &buf) == -1) {
			perror("stat");
			exit(EXIT_FAILURE);
		}

		if (S_ISDIR(buf.st_mode)) {
			printf("This is a directory\n");
		}
	}
	if (errno != 0) {
		perror("readdir");
		exit(EXIT_FAILURE);
	}

	closedir(dirp);
}
