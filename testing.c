#include <ctype.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

//// TOKENIZATION

typedef struct token token;

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
		IF_TOKEN,
		LOOP_TOKEN,
		BREAK_TOKEN,
		RETURN_TOKEN,
		CONTINUE_TOKEN,
		SPACES_TOKEN,
		NEWLINES_TOKEN,
		STRING_TOKEN,
		FIELD_NAME_TOKEN,
		TEXT_TOKEN,
		NUMBER_TOKEN,
		COMMENT_TOKEN,
	} type;
	char *start;
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
	[IF_TOKEN] = "IF_TOKEN",
	[LOOP_TOKEN] = "LOOP_TOKEN",
	[BREAK_TOKEN] = "BREAK_TOKEN",
	[RETURN_TOKEN] = "RETURN_TOKEN",
	[CONTINUE_TOKEN] = "CONTINUE_TOKEN",
	[SPACES_TOKEN] = "SPACES_TOKEN",
	[NEWLINES_TOKEN] = "NEWLINES_TOKEN",
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
static struct tokens tokens;

static size_t max_size_t(size_t a, size_t b) {
	if (a > b) {
		return a;
	}
	return b;
}

static void print_tokens() {
	size_t longest_token_type_len = 0;
	for (size_t i = 0; i < tokens.size; i++) {
		token token = tokens.tokens[i];
		char *token_type_str = get_token_type_str[token.type];
		longest_token_type_len = max_size_t(strlen(token_type_str), longest_token_type_len);
	}

	printf("| %-*s | str\n", (int)longest_token_type_len, "type");
	printf("| %-*s |\n", (int)longest_token_type_len, "");

	for (size_t i = 0; i < tokens.size; i++) {
		token token = tokens.tokens[i];
		char *token_type_str = get_token_type_str[token.type];
		printf("| %-*s ", (int)longest_token_type_len, token_type_str);

		if (token.type == NEWLINES_TOKEN) {
			printf("| '");
			for (size_t i = 0; i < token.len; i++) {
				printf("\\n");
			}
			printf("'\n");
		} else {
			printf("| '%.*s'\n", (int)token.len, token.start);
		}
	}
}

static void push_token(token token) {
	// Make sure there's enough room to push token
	if (tokens.size + 1 > tokens.capacity) {
		tokens.capacity = tokens.capacity == 0 ? 1 : tokens.capacity * 2;
		tokens.tokens = realloc(tokens.tokens, tokens.capacity * sizeof(*tokens.tokens));
		if (!tokens.tokens) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
	}

	tokens.tokens[tokens.size++] = token;
}

static void tokenize(char *grug_text) {
	size_t i = 0;
	while (grug_text[i]) {
		if (       grug_text[i] == '(') {
			push_token((token){.type=OPEN_PARENTHESIS_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ')') {
			push_token((token){.type=CLOSE_PARENTHESIS_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '{') {
			push_token((token){.type=OPEN_BRACE_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '}') {
			push_token((token){.type=CLOSE_BRACE_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '+') {
			push_token((token){.type=PLUS_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '-') {
			push_token((token){.type=MINUS_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ',') {
			push_token((token){.type=COMMA_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ':') {
			push_token((token){.type=COLON_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '=' && grug_text[i + 1] == '=') {
			push_token((token){.type=EQUALITY_TOKEN, .start=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '=') {
			push_token((token){.type=ASSIGNMENT_TOKEN, .start=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == 'i' && grug_text[i + 1] == 'f' && grug_text[i + 2] == ' ') {
			push_token((token){.type=IF_TOKEN, .start=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i + 0] == 'l' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'o' && grug_text[i + 3] == 'p' && grug_text[i + 4] == ' ') {
			push_token((token){.type=LOOP_TOKEN, .start=grug_text+i, .len=4});
			i += 4;
		} else if (grug_text[i + 0] == 'b' && grug_text[i + 1] == 'r' && grug_text[i + 2] == 'e' && grug_text[i + 3] == 'a' && grug_text[i + 4] == 'k' && grug_text[i + 5] == ' ') {
			push_token((token){.type=BREAK_TOKEN, .start=grug_text+i, .len=5});
			i += 5;
		} else if (grug_text[i + 0] == 'r' && grug_text[i + 1] == 'e' && grug_text[i + 2] == 't' && grug_text[i + 3] == 'u' && grug_text[i + 4] == 'r' && grug_text[i + 5] == 'n' && grug_text[i + 6] == ' ') {
			push_token((token){.type=RETURN_TOKEN, .start=grug_text+i, .len=6});
			i += 6;
		} else if (grug_text[i + 0] == 'c' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'n' && grug_text[i + 3] == 't' && grug_text[i + 4] == 'i' && grug_text[i + 5] == 'n' && grug_text[i + 6] == 'u' && grug_text[i + 7] == 'e' && grug_text[i + 8] == ' ') {
			push_token((token){.type=CONTINUE_TOKEN, .start=grug_text+i, .len=8});
			i += 8;
		} else if (grug_text[i] == ' ') {
			token token = {.type=SPACES_TOKEN, .start=grug_text+i};

			do {
				i++;
			} while (grug_text[i] == ' ');

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (grug_text[i] == '\n') {
			token token = {.type=NEWLINES_TOKEN, .start=grug_text+i};

			do {
				i++;
			} while (grug_text[i] == '\n');

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (grug_text[i] == '\"') {
			token token = {.type=STRING_TOKEN, .start=grug_text+i};

			do {
				i++;
			} while (grug_text[i] != '\"' && grug_text[i] != '\0');

			if (grug_text[i] == '\"') {
				i++;
			}

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (grug_text[i] == '.') {
			token token = {.type=FIELD_NAME_TOKEN, .start=grug_text+i};

			i++;

			// TODO: Decide if this should return an error value if the first char was a digit,
			// or anything else was wrong, like the input being ".."
			while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.') {
				i++;
			}

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (isalpha(grug_text[i]) || grug_text[i] == '_') {
			token token = {.type=TEXT_TOKEN, .start=grug_text+i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.');

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (isdigit(grug_text[i])) {
			token token = {.type=NUMBER_TOKEN, .start=grug_text+i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isdigit(grug_text[i]) || grug_text[i] == '.');

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else if (grug_text[i] == ';') {
			token token = {.type=COMMENT_TOKEN, .start=grug_text+i};

			do {
				i++;
			} while (grug_text[i] != '\n' && grug_text[i] != '\0');

			token.len = i - (token.start - grug_text);
			push_token(token);
		} else {
			fprintf(stderr, "Unrecognized character '%c' at index %zu\n", grug_text[i], i);
			exit(EXIT_FAILURE);
		}
	}
}

//// PARSING

typedef struct call_expr call_expr;
typedef struct unary_expr unary_expr;
typedef struct binary_expr binary_expr;
typedef struct literal literal;
typedef struct expr expr;
typedef struct return_statement return_statement;
typedef struct if_statement if_statement;
typedef struct statement statement;
typedef struct node node;
typedef struct argument argument;
typedef struct fn fn;

struct call_expr {
	char *fn_name;
	size_t fn_name_len;
	expr *arguments;
	size_t argument_count;
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

struct exprs {
	expr *exprs;
	size_t size;
	size_t capacity;
};
static struct exprs exprs;

struct return_statement {
	expr *value;
};

struct if_statement {
	expr condition;
	node *body;
	size_t body_count;
	node *else_body;
	size_t else_body_count;
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
static struct nodes nodes;

struct argument {
	char *type;
	char *name;
};

struct arguments {
	argument *arguments;
	size_t size;
	size_t capacity;
};
static struct arguments arguments;

struct fn {
	char *fn_name;
	size_t fn_name_len;
	argument *arguments;
	size_t argument_count;
	char *return_type;
	node *body;
	size_t body_count;
};

struct fns {
	fn *fns;
	size_t size;
	size_t capacity;
};
static struct fns fns;

// static char *serialize_to_c() {
// 	char *c_text;

// 	c_text = "";

// 	return c_text;
// }

static void print_fns() {
	printf("{\n");
	for (size_t fn_index = 0; fn_index < fns.size; fn_index++) {
		fn fn = fns.fns[fn_index];

		printf("\"fn_name\": \"%s\"\n", fn.fn_name);
		printf("\"fn_name_len\": %zu\n", fn.fn_name_len);
		printf("\"arguments\": [\n");
		for (size_t arg_index = 0; arg_index < fn.argument_count; arg_index++) {
			printf("{\n");
			printf("}\n");
		}
		printf("]\n");
		printf("\"argument_count\": %zu\n", fn.argument_count);
		printf("\"return_type\": \"%s\"\n", fn.return_type);
		printf("\"body\": [\n");
		for (size_t body_index = 0; body_index < fn.body_count; body_index++) {
			printf("{\n");
			printf("}\n");
		}
		printf("]\n");
		printf("\"body_count\": %zu\n", fn.body_count);
	}
	printf("}\n");
}

// static void push_argument(argument argument) {
// 	// Make sure there's enough room to push argument
// 	if (arguments.size + 1 > arguments.capacity) {
// 		arguments.capacity = arguments.capacity == 0 ? 1 : arguments.capacity * 2;
// 		arguments.arguments = realloc(arguments.arguments, arguments.capacity * sizeof(*arguments.arguments));
// 		if (!arguments.arguments) {
// 			perror("realloc");
// 			exit(EXIT_FAILURE);
// 		}
// 	}

// 	arguments.arguments[arguments.size++] = argument;
// }

static void push_fn(fn fn) {
	// Make sure there's enough room to push fn
	if (fns.size + 1 > fns.capacity) {
		fns.capacity = fns.capacity == 0 ? 1 : fns.capacity * 2;
		fns.fns = realloc(fns.fns, fns.capacity * sizeof(*fns.fns));
		if (!fns.fns) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
	}

	fns.fns[fns.size++] = fn;
}

void assert_token(size_t token_index, unsigned int expected_type) {
	token token = tokens.tokens[token_index];
	if (token.type != expected_type) {
		fprintf(stderr, "Expected token type %s, but got token type %s at token index %zu\n", get_token_type_str[expected_type], get_token_type_str[token.type], token_index);
		exit(EXIT_FAILURE);
	}
}

static void parse_fn(size_t *i) {
	fn fn = {0};

	token token = tokens.tokens[*i];
	fn.fn_name = token.start;
	fn.fn_name_len = token.len;
	(*i)++;

	token = tokens.tokens[*i];
	assert_token(*i, OPEN_PARENTHESIS_TOKEN);

	// while (*i < tokens.size) {
	// 	token token = tokens.tokens[*i];
	// 	int type = token.type;

	// 	if (       type == TEXT_TOKEN) {
	// 		parse_fn(&i);
	// 	} else if (type == COMMENT_TOKEN) {
	// 		i++;
	// 	} else if (type == NEWLINES_TOKEN) {
	// 		i++;
	// 	} else {
	// 		fprintf(stderr, "Unexpected token '%.*s' at token index %zu in parse_fn()\n", (int)token.len, token.start, i);
	// 		exit(EXIT_FAILURE);
	// 	}
	// }

	push_fn(fn);
}

static void parse() {
	size_t i = 0;
	while (i < tokens.size) {
		token token = tokens.tokens[i];
		int type = token.type;

		if (       type == TEXT_TOKEN) {
			parse_fn(&i);
		} else if (type == COMMENT_TOKEN) {
			i++;
		} else if (type == NEWLINES_TOKEN) {
			i++;
		} else {
			fprintf(stderr, "Unexpected token '%.*s' at token index %zu in parse()\n", (int)token.len, token.start, i);
			exit(EXIT_FAILURE);
		}
	}
}

static void init() {
	tokens.size = 0;
	exprs.size = 0;
	nodes.size = 0;
	arguments.size = 0;
	fns.size = 0;
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

	init();

	tokenize(grug_text);
	print_tokens();

	parse();
	printf("\nfns:\n");
	print_fns();

	// char *c_text = serialize_to_c();
	// printf("c_text:\n%s\n", c_text);

	free(arguments.arguments);
	free(fns.fns);
	free(tokens.tokens);
	free(grug_text);
}
