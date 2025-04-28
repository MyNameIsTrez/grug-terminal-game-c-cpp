#pragma once

#define MAX_GAME_FUNCTION_ERROR_CHARS 420
extern char game_function_error[MAX_GAME_FUNCTION_ERROR_CHARS];

#define GAME_FUNCTION_ERROR(...) { \
	snprintf(game_function_error, sizeof(game_function_error), __VA_ARGS__); \
	grug_game_function_error_happened(game_function_error); \
}
