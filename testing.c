#include <ctype.h>
#include <stdbool.h>
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
			fprintf(stderr, "Unrecognized character '%c' at grug_text index %zu\n", grug_text[i], i);
			exit(EXIT_FAILURE);
		}
	}
}

//// PARSING

#define SPACES_PER_INDENT 4

typedef struct call_expr call_expr;
typedef struct unary_expr unary_expr;
typedef struct binary_expr binary_expr;
typedef struct entry entry;
typedef struct compound_literal compound_literal;
typedef struct literal literal;
typedef struct expr expr;
typedef struct return_statement return_statement;
typedef struct if_statement if_statement;
typedef struct statement statement;
typedef struct node node;
typedef struct argument argument;
typedef struct helper_fn helper_fn;
typedef struct on_fn on_fn;
typedef struct define_fn define_fn;

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

struct entry {
	char *key;
	size_t key_len;
	char *value;
	size_t value_len;
};

struct entries {
	entry *entries;
	size_t size;
	size_t capacity;
};
static struct entries entries;

struct compound_literal {
	entry *entries;
	size_t entry_count;
};

struct literal {
	char *str;
	size_t len;
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
		compound_literal compound_literal;
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
	size_t variable_name_len;
	char *type;
	size_t type_len;
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
	size_t type_len;
	char *name;
	size_t name_len;
};

struct arguments {
	argument *arguments;
	size_t size;
	size_t capacity;
};
static struct arguments arguments;

struct helper_fn {
	char *fn_name;
	size_t fn_name_len;
	argument *arguments;
	size_t argument_count;
	char *return_type;
	size_t return_type_len;
	node *body;
	size_t body_count;
};

struct helper_fns {
	helper_fn *fns;
	size_t size;
	size_t capacity;
};
static struct helper_fns helper_fns;

struct on_fn {
	char *fn_name;
	size_t fn_name_len;
	argument *arguments;
	size_t argument_count;
	node *body;
	size_t body_count;
};

struct on_fns {
	on_fn *fns;
	size_t size;
	size_t capacity;
};
static struct on_fns on_fns;

struct define_fn {
	char *fn_name;
	size_t fn_name_len;
	char *return_type;
	size_t return_type_len;
	compound_literal returned_compound_literal;
};

struct define_fns {
	define_fn *fns;
	size_t size;
	size_t capacity;
};
static struct define_fns define_fns;

// static char *serialize_to_c() {
// 	char *c_text;

// 	c_text = "";

// 	return c_text;
// }

static void print_helper_fns() {
}

static void print_on_fns() {
}

static void print_compound_literal(compound_literal compound_literal) {
	printf("\"returned_compound_literal\": {\n");

	for (size_t entry_index = 0; entry_index < compound_literal.entry_count; entry_index++) {
		entry entry = compound_literal.entries[entry_index];

		printf("\"key\": \"%.*s\"\n", (int)entry.key_len, entry.key);
		printf("\"value\": \"%.*s\"\n", (int)entry.value_len, entry.value);
	}

	printf("}\n");
}

static void print_define_fns() {
	printf("\"define_fns\": {\n");

	for (size_t fn_index = 0; fn_index < define_fns.size; fn_index++) {
		define_fn fn = define_fns.fns[fn_index];

		printf("\"fn_name\": \"%.*s\"\n", (int)fn.fn_name_len, fn.fn_name);
		printf("\"fn_name_len\": %zu\n", fn.fn_name_len);
		printf("\"return_type\": \"%.*s\"\n", (int)fn.return_type_len, fn.return_type);
		printf("\"return_type_len\": %zu\n", fn.return_type_len);

		print_compound_literal(fn.returned_compound_literal);
	}

	printf("}\n");
}

static void print_fns() {
	printf("{\n");

	print_define_fns();
	print_on_fns();
	print_helper_fns();

	printf("}\n");
}

static void parse_helper_fn(size_t *i) {
	(void)i;
}

static void parse_on_fn(size_t *i) {
	(void)i;
}

static void push_define_fn(define_fn fn) {
	// Make sure there's enough room to push fn
	if (define_fns.size + 1 > define_fns.capacity) {
		define_fns.capacity = define_fns.capacity == 0 ? 1 : define_fns.capacity * 2;
		define_fns.fns = realloc(define_fns.fns, define_fns.capacity * sizeof(*define_fns.fns));
		if (!define_fns.fns) {
			perror("realloc");
			exit(EXIT_FAILURE);
		}
	}

	define_fns.fns[define_fns.size++] = fn;
}

static void assert_token_type(size_t token_index, unsigned int expected_type) {
	token token = tokens.tokens[token_index];
	if (token.type != expected_type) {
		fprintf(stderr, "Expected token type %s, but got %s at token index %zu\n", get_token_type_str[expected_type], get_token_type_str[token.type], token_index);
		exit(EXIT_FAILURE);
	}
}

static void assert_spaces(size_t token_index, size_t expected_spaces) {
	assert_token_type(token_index, SPACES_TOKEN);

	token token = tokens.tokens[token_index];
	if (token.len != expected_spaces) {
		fprintf(stderr, "Expected %zu space%s, but got %zu at token index %zu\n", expected_spaces, expected_spaces > 1 ? "s" : "", token.len, token_index);
		exit(EXIT_FAILURE);
	}
}

static void parse_define_fn(size_t *i) {
	define_fn fn = {0};

	token token = tokens.tokens[*i];
	fn.fn_name = token.start;
	fn.fn_name_len = token.len;
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, OPEN_PARENTHESIS_TOKEN);
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, CLOSE_PARENTHESIS_TOKEN);
	(*i)++;

	token = tokens.tokens[*i];
	assert_spaces(*i, 1);
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, TEXT_TOKEN);
	fn.return_type = token.start;
	fn.return_type_len = token.len;
	(*i)++;

	token = tokens.tokens[*i];
	assert_spaces(*i, 1);
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, OPEN_BRACE_TOKEN);
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, NEWLINES_TOKEN);
	(*i)++;

	token = tokens.tokens[*i];
	assert_spaces(*i, 4);
	size_t depth = 1; // TODO: Don't hardcode!
	if (token.len != depth * SPACES_PER_INDENT) {
		fprintf(stderr, "Expected %zu spaces, but got %zu spaces at token index %zu\n", depth * SPACES_PER_INDENT, token.len, *i);
		exit(EXIT_FAILURE);
	}
	(*i)++;

	token = tokens.tokens[*i];
	assert_token_type(*i, RETURN_TOKEN);
	(*i)++;

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

	push_define_fn(fn);
}

static bool starts_with(char *a, char *b) {
	return strncmp(a, b, strlen(b)) == 0;
}

static void parse() {
	size_t i = 0;
	while (i < tokens.size) {
		token token = tokens.tokens[i];
		int type = token.type;

		if (       type == TEXT_TOKEN && starts_with(token.start, "define_")) {
			parse_define_fn(&i);
		} else if (type == TEXT_TOKEN && starts_with(token.start, "on_")) {
			parse_on_fn(&i);
		} else if (type == TEXT_TOKEN) {
			parse_helper_fn(&i);
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

static void grug_free() {
	free(tokens.tokens);
	free(entries.entries);
	free(exprs.exprs);
	free(nodes.nodes);
	free(arguments.arguments);
	free(helper_fns.fns);
	free(on_fns.fns);
	free(define_fns.fns);
}

static void reset() {
	tokens.size = 0;
	entries.size = 0;
	exprs.size = 0;
	nodes.size = 0;
	arguments.size = 0;
	helper_fns.size = 0;
	on_fns.size = 0;
	define_fns.size = 0;
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

	reset();

	tokenize(grug_text);
	print_tokens();

	parse();
	printf("\nfns:\n");
	print_fns();

	// char *c_text = serialize_to_c();
	// printf("c_text:\n%s\n", c_text);

	grug_free();
	free(grug_text);
}
