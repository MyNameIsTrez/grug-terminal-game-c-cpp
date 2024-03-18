#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct token token;
typedef struct tokens tokens;

struct token {
	enum {
		OPEN_PARENTHESIS_TOKEN,
		CLOSE_PARENTHESIS_TOKEN,
		OPEN_BRACE_TOKEN,
		CLOSE_BRACE_TOKEN,
		PLUS_TOKEN,
		MINUS_TOKEN,
		COMMA_TOKEN,
		COLON_TOKEN,
		EQUALITY_TOKEN,
		ASSIGNMENT_TOKEN,
		SPACE_TOKEN,
		NEWLINE_TOKEN,
		TAB_TOKEN,
		IF_TOKEN,
		LOOP_TOKEN,
		BREAK_TOKEN,
		RETURN_TOKEN,
		CONTINUE_TOKEN,
		STRING_TOKEN,
		FIELD_NAME_TOKEN,
		TEXT_TOKEN,
		NUMBER_TOKEN,
		COMMENT_TOKEN,
	} type;
	size_t start;
	size_t len;
};

static char *get_token_type_str[] = {
	[OPEN_PARENTHESIS_TOKEN] = "OPEN_PARENTHESIS_TOKEN",
	[CLOSE_PARENTHESIS_TOKEN] = "CLOSE_PARENTHESIS_TOKEN",
	[OPEN_BRACE_TOKEN] = "OPEN_BRACE_TOKEN",
	[CLOSE_BRACE_TOKEN] = "CLOSE_BRACE_TOKEN",
	[PLUS_TOKEN] = "PLUS_TOKEN",
	[MINUS_TOKEN] = "MINUS_TOKEN",
	[COMMA_TOKEN] = "COMMA_TOKEN",
	[COLON_TOKEN] = "COLON_TOKEN",
	[EQUALITY_TOKEN] = "EQUALITY_TOKEN",
	[ASSIGNMENT_TOKEN] = "ASSIGNMENT_TOKEN",
	[SPACE_TOKEN] = "SPACE_TOKEN",
	[NEWLINE_TOKEN] = "NEWLINE_TOKEN",
	[TAB_TOKEN] = "TAB_TOKEN",
	[IF_TOKEN] = "IF_TOKEN",
	[LOOP_TOKEN] = "LOOP_TOKEN",
	[BREAK_TOKEN] = "BREAK_TOKEN",
	[RETURN_TOKEN] = "RETURN_TOKEN",
	[CONTINUE_TOKEN] = "CONTINUE_TOKEN",
	[STRING_TOKEN] = "STRING_TOKEN",
	[FIELD_NAME_TOKEN] = "FIELD_NAME_TOKEN",
	[TEXT_TOKEN] = "TEXT_TOKEN",
	[NUMBER_TOKEN] = "NUMBER_TOKEN",
	[COMMENT_TOKEN] = "COMMENT_TOKEN",
};

struct tokens {
	token *tokens;
	size_t size;
	size_t capacity;
};

static void push_token(tokens *tokens, token token) {
	// Make sure there's enough room to push token
	if (tokens->size + 1 > tokens->capacity) {
		tokens->capacity = tokens->capacity == 0 ? 1 : tokens->capacity * 2;
		tokens->tokens = realloc(tokens->tokens, tokens->capacity * sizeof(*tokens->tokens));
		if (!tokens->tokens) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
	}

	tokens->tokens[tokens->size++] = token;
}

static tokens tokenize(char *grug_text) {
	tokens tokens = {0};

	size_t i = 0;
	while (grug_text[i]) {
		if (       grug_text[i + 0] == '(') {
			push_token(&tokens, (token){.type=OPEN_PARENTHESIS_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == ')') {
			push_token(&tokens, (token){.type=CLOSE_PARENTHESIS_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '{') {
			push_token(&tokens, (token){.type=OPEN_BRACE_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '}') {
			push_token(&tokens, (token){.type=CLOSE_BRACE_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '+') {
			push_token(&tokens, (token){.type=PLUS_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '-') {
			push_token(&tokens, (token){.type=MINUS_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == ',') {
			push_token(&tokens, (token){.type=COMMA_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == ':') {
			push_token(&tokens, (token){.type=COLON_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '=' && grug_text[i + 1] == '=') {
			push_token(&tokens, (token){.type=EQUALITY_TOKEN, .start=i, .len=2});
			i += 2;
		} else if (grug_text[i + 0] == '=') {
			push_token(&tokens, (token){.type=ASSIGNMENT_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == ' ') {
			push_token(&tokens, (token){.type=SPACE_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '\n') {
			push_token(&tokens, (token){.type=NEWLINE_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == '\t') {
			push_token(&tokens, (token){.type=TAB_TOKEN, .start=i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == 'i' && grug_text[i + 1] == 'f' && grug_text[i + 2] == ' ') {
			push_token(&tokens, (token){.type=IF_TOKEN, .start=i, .len=2});
			i += 2;
		} else if (grug_text[i + 0] == 'l' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'o' && grug_text[i + 3] == 'p' && grug_text[i + 4] == ' ') {
			push_token(&tokens, (token){.type=LOOP_TOKEN, .start=i, .len=4});
			i += 4;
		} else if (grug_text[i + 0] == 'b' && grug_text[i + 1] == 'r' && grug_text[i + 2] == 'e' && grug_text[i + 3] == 'a' && grug_text[i + 4] == 'k' && grug_text[i + 5] == ' ') {
			push_token(&tokens, (token){.type=BREAK_TOKEN, .start=i, .len=5});
			i += 5;
		} else if (grug_text[i + 0] == 'r' && grug_text[i + 1] == 'e' && grug_text[i + 2] == 't' && grug_text[i + 3] == 'u' && grug_text[i + 4] == 'r' && grug_text[i + 5] == 'n' && grug_text[i + 6] == ' ') {
			push_token(&tokens, (token){.type=RETURN_TOKEN, .start=i, .len=6});
			i += 6;
		} else if (grug_text[i + 0] == 'c' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'n' && grug_text[i + 3] == 't' && grug_text[i + 4] == 'i' && grug_text[i + 5] == 'n' && grug_text[i + 6] == 'u' && grug_text[i + 7] == 'e' && grug_text[i + 8] == ' ') {
			push_token(&tokens, (token){.type=CONTINUE_TOKEN, .start=i, .len=8});
			i += 8;
		} else if (grug_text[i] == '\"') {
			token token = {.type=STRING_TOKEN, .start=i};

			do {
				i++;
			} while (grug_text[i] != '\"' && grug_text[i] != '\0');

			if (grug_text[i] == '\"') {
				i++;
			}

			token.len = i - token.start;
			push_token(&tokens, token);
		} else if (grug_text[i] == '.') {
			token token = {.type=FIELD_NAME_TOKEN, .start=i};

			i++;

			// TODO: Decide if this should return an error value if the first char was a digit,
			// or anything else was wrong, like the input being ".."
			while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.') {
				i++;
			}

			token.len = i - token.start;
			push_token(&tokens, token);
		} else if (isalpha(grug_text[i]) || grug_text[i] == '_') {
			token token = {.type=TEXT_TOKEN, .start=i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.');

			token.len = i - token.start;
			push_token(&tokens, token);
		} else if (isdigit(grug_text[i])) {
			token token = {.type=NUMBER_TOKEN, .start=i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isdigit(grug_text[i]) || grug_text[i] == '.');

			token.len = i - token.start;
			push_token(&tokens, token);
		} else if (grug_text[i] == ';') {
			token token = {.type=COMMENT_TOKEN, .start=i};

			do {
				i++;
			} while (grug_text[i] != '\n' && grug_text[i] != '\0');

			token.len = i - token.start;
			push_token(&tokens, token);
		}
	}

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

// clang testing.c -Wall -Wextra -Werror -Wpedantic -Wfatal-errors -fsanitize=address,undefined -g && ./a.out
int main() {
	char *grug_text = read_file("zombie.grug");

	printf("grug_text:\n%s\n", grug_text);

	tokens tokens = tokenize(grug_text);

	for (size_t i = 0; i < tokens.size; i++) {
		token token = tokens.tokens[i];
		printf("type: %s, str: '%.*s'\n", get_token_type_str[token.type], (int)token.len, grug_text + token.start);
	}

	free(tokens.tokens);
	free(grug_text);
}
