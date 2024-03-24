#include <ctype.h>
#include <errno.h>
#include <setjmp.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#define MAX_TOKENS_IN_FILE 420420
#define MAX_FIELDS_IN_FILE 420420
#define MAX_EXPRS_IN_FILE 420420
#define MAX_STATEMENTS_IN_FILE 420420
#define MAX_ARGUMENTS_IN_FILE 420420
#define MAX_HELPER_FNS_IN_FILE 420420
#define MAX_ON_FNS_IN_FILE 420420
#define SPACES_PER_INDENT 4
#define MAX_STATEMENTS_PER_STACK_FRAME 1337

static char error_msg[420];
jmp_buf jmp_buffer;

typedef void (*grug_error_handler_fn)(char *error_msg);
grug_error_handler_fn grug_error_handler;

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
		ELSE_TOKEN,
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
	char *str;
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
	[ELSE_TOKEN] = "ELSE_TOKEN",
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
static token tokens[MAX_TOKENS_IN_FILE];
static size_t tokens_size;

static size_t max_size_t(size_t a, size_t b) {
	if (a > b) {
		return a;
	}
	return b;
}

static token get_token(size_t token_index) {
	if (token_index >= tokens_size) {
		snprintf(error_msg, sizeof(error_msg), "token_index %zu was out of bounds in get_token()", token_index);
		longjmp(jmp_buffer, 1);
	}
	return tokens[token_index];
}

static void print_tokens() {
	size_t longest_token_type_len = 0;
	for (size_t i = 0; i < tokens_size; i++) {
		token token = get_token(i);
		char *token_type_str = get_token_type_str[token.type];
		longest_token_type_len = max_size_t(strlen(token_type_str), longest_token_type_len);
	}

	// Leave enough space for the word "index", but if the index exceeds 99999, add extra spaces
	// In pseudocode this does longest_index = max(floor(log10(tokens.size)), strlen("index"))
	size_t longest_index = 1;
	size_t n = tokens_size;
	while (true) {
		n /= 10;
		if (n == 0) {
			break;
		}
		longest_index++;
	}
	longest_index = max_size_t(longest_index, strlen("index"));

	printf("| %-*s | %-*s | str\n", (int)longest_index, "index", (int)longest_token_type_len, "type");

	for (size_t i = 0; i < tokens_size; i++) {
		token token = get_token(i);

		printf("| %*zu ", (int)longest_index, i);

		char *token_type_str = get_token_type_str[token.type];
		printf("| %*s ", (int)longest_token_type_len, token_type_str);

		if (token.type == NEWLINES_TOKEN) {
			printf("| '");
			for (size_t i = 0; i < token.len; i++) {
				printf("\\n");
			}
			printf("'\n");
		} else {
			printf("| '%.*s'\n", (int)token.len, token.str);
		}
	}
}

static char *get_escaped_char(char *str) {
	switch (*str) {
	case '\f':
		return "\\f";
	case '\n':
		return "\\n";
	case '\r':
		return "\\r";
	case '\t':
		return "\\t";
	case '\v':
		return "\\v";
	}
	return str;
}

static bool is_escaped_char(char c) {
	return isspace(c) && c != ' ';
}

static void push_token(token token) {
	// Make sure there's enough room to push token
	if (tokens_size + 1 > MAX_TOKENS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d tokens in the grug file, exceeding MAX_TOKENS_IN_FILE!", MAX_TOKENS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	tokens[tokens_size++] = token;
}

static void tokenize(char *grug_text) {
	size_t i = 0;
	while (grug_text[i]) {
		if (       grug_text[i] == '(') {
			push_token((token){.type=OPEN_PARENTHESIS_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ')') {
			push_token((token){.type=CLOSE_PARENTHESIS_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '{') {
			push_token((token){.type=OPEN_BRACE_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '}') {
			push_token((token){.type=CLOSE_BRACE_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '+') {
			push_token((token){.type=PLUS_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '-') {
			push_token((token){.type=MINUS_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ',') {
			push_token((token){.type=COMMA_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ':') {
			push_token((token){.type=COLON_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '=' && grug_text[i + 1] == '=') {
			push_token((token){.type=EQUALITY_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '=') {
			push_token((token){.type=ASSIGNMENT_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == 'i' && grug_text[i + 1] == 'f' && grug_text[i + 2] == ' ') {
			push_token((token){.type=IF_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i + 0] == 'e' && grug_text[i + 1] == 'l' && grug_text[i + 2] == 's' && grug_text[i + 3] == 'e' && grug_text[i + 4] == ' ') {
			push_token((token){.type=ELSE_TOKEN, .str=grug_text+i, .len=4});
			i += 4;
		} else if (grug_text[i + 0] == 'l' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'o' && grug_text[i + 3] == 'p' && grug_text[i + 4] == ' ') {
			push_token((token){.type=LOOP_TOKEN, .str=grug_text+i, .len=4});
			i += 4;
		} else if (grug_text[i + 0] == 'b' && grug_text[i + 1] == 'r' && grug_text[i + 2] == 'e' && grug_text[i + 3] == 'a' && grug_text[i + 4] == 'k' && (grug_text[i + 5] == ' ' || grug_text[i + 5] == '\n')) {
			push_token((token){.type=BREAK_TOKEN, .str=grug_text+i, .len=5});
			i += 5;
		} else if (grug_text[i + 0] == 'r' && grug_text[i + 1] == 'e' && grug_text[i + 2] == 't' && grug_text[i + 3] == 'u' && grug_text[i + 4] == 'r' && grug_text[i + 5] == 'n' && (grug_text[i + 6] == ' ' || grug_text[i + 6] == '\n')) {
			push_token((token){.type=RETURN_TOKEN, .str=grug_text+i, .len=6});
			i += 6;
		} else if (grug_text[i + 0] == 'c' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 'n' && grug_text[i + 3] == 't' && grug_text[i + 4] == 'i' && grug_text[i + 5] == 'n' && grug_text[i + 6] == 'u' && grug_text[i + 7] == 'e' && (grug_text[i + 8] == ' ' || grug_text[i + 8] == '\n')) {
			push_token((token){.type=CONTINUE_TOKEN, .str=grug_text+i, .len=8});
			i += 8;
		} else if (grug_text[i] == ' ') {
			token token = {.type=SPACES_TOKEN, .str=grug_text+i};

			do {
				i++;
			} while (grug_text[i] == ' ');

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (grug_text[i] == '\n') {
			token token = {.type=NEWLINES_TOKEN, .str=grug_text+i};

			do {
				i++;
			} while (grug_text[i] == '\n');

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (grug_text[i] == '\"') {
			token token = {.type=STRING_TOKEN, .str=grug_text+i};

			do {
				i++;
			} while (grug_text[i] != '\"' && grug_text[i] != '\0');

			if (grug_text[i] == '\"') {
				i++;
			}

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (grug_text[i] == '.') {
			token token = {.type=FIELD_NAME_TOKEN, .str=grug_text+i};

			i++;

			// TODO: Decide if this should return an error value if the first char was a digit,
			// or anything else was wrong, like the input being ".."
			while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.') {
				i++;
			}

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (isalpha(grug_text[i]) || grug_text[i] == '_') {
			token token = {.type=TEXT_TOKEN, .str=grug_text+i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isalnum(grug_text[i]) || grug_text[i] == '_' || grug_text[i] == '.');

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (isdigit(grug_text[i])) {
			token token = {.type=NUMBER_TOKEN, .str=grug_text+i};

			// TODO: Decide if this should return an error value when the input is ".."
			do {
				i++;
			} while (isdigit(grug_text[i]) || grug_text[i] == '.');

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else if (grug_text[i] == ';') {
			token token = {.type=COMMENT_TOKEN, .str=grug_text+i};

			while (true) {
				i++;
				if (!isprint(grug_text[i])) {
					if (grug_text[i] == '\n' || grug_text[i] == '\0') {
						break;
					}

					snprintf(error_msg, sizeof(error_msg), "Unexpected unprintable character '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[i]) ? 2 : 1, get_escaped_char(&grug_text[i]), i + 1);
					longjmp(jmp_buffer, 1);
				}
			}

			token.len = i - (token.str - grug_text);

			size_t comment_index = token.str - grug_text;
			if (token.len < 2 || token.str[1] != ' ')
			{
				snprintf(error_msg, sizeof(error_msg), "Expected the comment to str with a space character, but found '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[comment_index + 1]) ? 2 : 1, get_escaped_char(&grug_text[comment_index + 1]), comment_index + 2);
				longjmp(jmp_buffer, 1);
			}

			if (token.len < 3 || isspace(token.str[2]))
			{
				snprintf(error_msg, sizeof(error_msg), "Expected the comment to have a text character directly after the space, but found '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[comment_index + 2]) ? 2 : 1, get_escaped_char(&grug_text[comment_index + 2]), comment_index + 3);
				longjmp(jmp_buffer, 1);
			}

			if (isspace(token.str[token.len - 1]))
			{
				snprintf(error_msg, sizeof(error_msg), "Unexpected trailing whitespace '%.*s' at the end of the comment at character %zu of the grug text file", is_escaped_char(grug_text[comment_index + 1]) ? 2 : 1, get_escaped_char(&grug_text[comment_index + 1]), comment_index + token.len);
				longjmp(jmp_buffer, 1);
			}

			push_token(token);
		} else {
			snprintf(error_msg, sizeof(error_msg), "Unrecognized character '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[i]) ? 2 : 1, get_escaped_char(&grug_text[i]), i + 1);
			longjmp(jmp_buffer, 1);
		}
	}
}

//// PARSING

typedef struct literal_expr literal_expr;
typedef struct unary_expr unary_expr;
typedef struct binary_expr binary_expr;
typedef struct call_expr call_expr;
typedef struct field field;
typedef struct compound_literal compound_literal;
typedef struct expr expr;
typedef struct variable_statement variable_statement;
typedef struct if_statement if_statement;
typedef struct return_statement return_statement;
typedef struct loop_statement loop_statement;
typedef struct statement statement;
typedef struct argument argument;
typedef struct on_fn on_fn;
typedef struct helper_fn helper_fn;

struct literal_expr {
	char *str;
	size_t len;
};

struct unary_expr {
	char operator;
	size_t expr_index;
};

struct binary_expr {
	char operator;
	size_t left_expr_index;
	size_t right_expr_index;
};

struct call_expr {
	char *fn_name;
	size_t fn_name_len;
	size_t arguments_expr_offset;
	size_t argument_count;
};

struct field {
	char *key;
	size_t key_len;
	char *value;
	size_t value_len;
};
static field fields[MAX_FIELDS_IN_FILE];
static size_t fields_size;

struct compound_literal {
	size_t fields_offset;
	size_t field_count;
};

struct expr {
	enum {
		LITERAL_EXPR,
		UNARY_EXPR,
		BINARY_EXPR,
		CALL_EXPR,
	} type;
	union {
		literal_expr literal_expr;
		unary_expr unary_expr;
		binary_expr binary_expr;
		call_expr call_expr;
		compound_literal compound_literal;
	};
};
static char *get_expr_type_str[] = {
	[LITERAL_EXPR] = "LITERAL_EXPR",
	[UNARY_EXPR] = "UNARY_EXPR",
	[BINARY_EXPR] = "BINARY_EXPR",
	[CALL_EXPR] = "CALL_EXPR",
};
static expr exprs[MAX_EXPRS_IN_FILE];
static size_t exprs_size;

struct variable_statement {
	char *variable_name;
	size_t variable_name_len;
	char *type;
	size_t type_len;
	size_t value_expr_index;
};

struct if_statement {
	expr condition;
	size_t if_body_statements_offset;
	size_t if_body_statement_count;
	size_t else_body_statements_offset;
	size_t else_body_statement_count;
};

struct return_statement {
	size_t value_expr_index;
};

struct loop_statement {
	size_t body_statements_offset;
	size_t body_statement_count;
};

struct statement {
	enum {
		VARIABLE_STATEMENT,
		IF_STATEMENT,
		RETURN_STATEMENT,
		LOOP_STATEMENT,
		BREAK_STATEMENT,
		CONTINUE_STATEMENT,
	} type;
	union {
		variable_statement variable_statement;
		if_statement if_statement;
		return_statement return_statement;
		loop_statement loop_statement;
	};
};
static char *get_statement_type_str[] = {
	[VARIABLE_STATEMENT] = "VARIABLE_STATEMENT",
	[IF_STATEMENT] = "IF_STATEMENT",
	[RETURN_STATEMENT] = "RETURN_STATEMENT",
	[LOOP_STATEMENT] = "LOOP_STATEMENT",
	[BREAK_STATEMENT] = "BREAK_STATEMENT",
	[CONTINUE_STATEMENT] = "CONTINUE_STATEMENT",
};
static statement statements[MAX_STATEMENTS_IN_FILE];
static size_t statements_size;

struct argument {
	char *type;
	size_t type_len;
	char *name;
	size_t name_len;
};
static argument arguments[MAX_ARGUMENTS_IN_FILE];
static size_t arguments_size;

struct define_fn {
	char *fn_name;
	size_t fn_name_len;
	char *return_type;
	size_t return_type_len;
	compound_literal returned_compound_literal;
};
static struct define_fn define_fn;

struct on_fn {
	char *fn_name;
	size_t fn_name_len;
	size_t arguments_offset;
	size_t argument_count;
	size_t body_statements_offset;
	size_t body_statement_count;
};
static on_fn on_fns[MAX_ON_FNS_IN_FILE];
static size_t on_fns_size;

struct helper_fn {
	char *fn_name;
	size_t fn_name_len;
	size_t arguments_offset;
	size_t argument_count;
	char *return_type;
	size_t return_type_len;
	size_t body_statements_offset;
	size_t body_statement_count;
};
static helper_fn helper_fns[MAX_HELPER_FNS_IN_FILE];
static size_t helper_fns_size;

// static char *serialize_to_c() {
// 	char *c_text;

// 	c_text = "";

// 	return c_text;
// }

static void print_helper_fns() {
}

static void print_call_expr() {
	abort();
}

static void print_binary_expr() {
	abort();
}

static void print_unary_expr() {
	abort();
}

static void print_literal_expr(literal_expr literal_expr) {
	printf("\"str\": %.*s,\n", (int)literal_expr.len, literal_expr.str);
}

static void print_expr(expr expr) {
	printf("\"expr\": {\n");

	printf("\"type\": \"%s\",\n", get_expr_type_str[expr.type]);

	switch (expr.type) {
		case LITERAL_EXPR:
			print_literal_expr(expr.literal_expr);
			break;
		case UNARY_EXPR:
			print_unary_expr();
			break;
		case BINARY_EXPR:
			print_binary_expr();
			break;
		case CALL_EXPR:
			print_call_expr();
			break;
	}

	printf("},\n");
}

static void print_statements(size_t statements_offset, size_t statement_count) {
	printf("\"statements\": [\n");

	for (size_t statement_index = 0; statement_index < statement_count; statement_index++) {
		printf("{\n");

		statement st = statements[statements_offset + statement_index];

		printf("\"type\": \"%s\",\n", get_statement_type_str[st.type]);

		switch (st.type) {
			case VARIABLE_STATEMENT:
				break;
			case IF_STATEMENT:
				break;
			case RETURN_STATEMENT:
			{
				expr return_expr = exprs[st.return_statement.value_expr_index];
				print_expr(return_expr);
				break;
			}
			case LOOP_STATEMENT:
				print_statements(st.loop_statement.body_statements_offset, st.loop_statement.body_statement_count);
				break;
			case BREAK_STATEMENT:
				break;
			case CONTINUE_STATEMENT:
				break;
		}

		printf("},\n");
	}

	printf("],\n");
}

static void print_arguments(size_t arguments_offset, size_t argument_count) {
	printf("\"arguments\": [\n");

	for (size_t argument_index = 0; argument_index < argument_count; argument_index++) {
		printf("{\n");

		argument arg = arguments[arguments_offset + argument_index];

		printf("\"name\": \"%.*s\",\n", (int)arg.name_len, arg.name);
		printf("\"type\": \"%.*s\",\n", (int)arg.type_len, arg.type);

		printf("},\n");
	}

	printf("],\n");
}

static void print_on_fns() {
	printf("\"on_fns\": [\n");

	for (size_t fn_index = 0; fn_index < on_fns_size; fn_index++) {
		printf("{\n");

		on_fn fn = on_fns[fn_index];

		printf("\"fn_name\": \"%.*s\",\n", (int)fn.fn_name_len, fn.fn_name);

		print_arguments(fn.arguments_offset, fn.argument_count);
		print_statements(fn.body_statements_offset, fn.body_statement_count);

		printf("},\n");
	}

	printf("],\n");
}

static void print_compound_literal(compound_literal compound_literal) {
	printf("\"returned_compound_literal\": [\n");

	for (size_t field_index = 0; field_index < compound_literal.field_count; field_index++) {
		printf("{\n");

		field field = fields[compound_literal.fields_offset + field_index];

		printf("\"key\": \"%.*s\",\n", (int)field.key_len, field.key);
		printf("\"value\": %.*s,\n", (int)field.value_len, field.value);

		printf("},\n");
	}

	printf("]\n");
}

static void print_define_fn() {
	printf("\"define_fn\": {\n");

	printf("\"fn_name\": \"%.*s\",\n", (int)define_fn.fn_name_len, define_fn.fn_name);
	printf("\"return_type\": \"%.*s\",\n", (int)define_fn.return_type_len, define_fn.return_type);

	print_compound_literal(define_fn.returned_compound_literal);

	printf("},\n");
}

static void print_fns() {
	printf("{\n");

	print_define_fn();
	print_on_fns();
	print_helper_fns();

	printf("}\n");
}

static void parse_helper_fn(size_t *i) {
	(*i)++;
}

static void push_on_fn(on_fn on_fn) {
	// Make sure there's enough room to push on_fn
	if (on_fns_size + 1 > MAX_ON_FNS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d on_fns in the grug file, exceeding MAX_ON_FNS_IN_FILE!", MAX_ON_FNS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	on_fns[on_fns_size++] = on_fn;
}

static void push_statement(statement statement) {
	// Make sure there's enough room to push statement
	if (statements_size + 1 > MAX_STATEMENTS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d statements in the grug file, exceeding MAX_STATEMENTS_IN_FILE!", MAX_STATEMENTS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	statements[statements_size++] = statement;
}

static void push_expr(expr expr) {
	// Make sure there's enough room to push expr
	if (exprs_size + 1 > MAX_EXPRS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d exprs in the grug file, exceeding MAX_EXPRS_IN_FILE!", MAX_EXPRS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	exprs[exprs_size++] = expr;
}

static void skip_any_comment(size_t *i) {
	// If there is a comment token with exactly one space before it, skip them
	token space_token = get_token(*i);
	if (space_token.type == SPACES_TOKEN) {
		token comment_token = get_token(*i + 1);
		if (comment_token.type == COMMENT_TOKEN) {
			if (space_token.len == 1) {
				(*i) += 2;
			} else {
				snprintf(error_msg, sizeof(error_msg), "There were too many spaces before the comment at token index %zu", *i);
				longjmp(jmp_buffer, 1);
			}
		} else if (comment_token.type == SPACES_TOKEN) {
			snprintf(error_msg, sizeof(error_msg), "There was a trailing space at token index %zu", *i);
			longjmp(jmp_buffer, 1);
		} else {
			snprintf(error_msg, sizeof(error_msg), "Expected token type COMMENT_TOKEN, but got %s at token index %zu", get_token_type_str[comment_token.type], *i);
			longjmp(jmp_buffer, 1);
		}
	}
}

static void assert_token_type(size_t token_index, unsigned int expected_type) {
	token token = get_token(token_index);
	if (token.type != expected_type) {
		snprintf(error_msg, sizeof(error_msg), "Expected token type %s, but got %s at token index %zu", get_token_type_str[expected_type], get_token_type_str[token.type], token_index);
		longjmp(jmp_buffer, 1);
	}
}

static void assert_1_newline(size_t token_index) {
	assert_token_type(token_index, NEWLINES_TOKEN);

	token token = get_token(token_index);
	if (token.len != 1) {
		snprintf(error_msg, sizeof(error_msg), "Expected 1 newline, but got %zu at token index %zu", token.len, token_index);
		longjmp(jmp_buffer, 1);
	}
}

static void assert_spaces(size_t token_index, size_t expected_spaces) {
	assert_token_type(token_index, SPACES_TOKEN);

	token token = get_token(token_index);
	if (token.len != expected_spaces) {
		snprintf(error_msg, sizeof(error_msg), "Expected %zu space%s, but got %zu at token index %zu", expected_spaces, expected_spaces > 1 ? "s" : "", token.len, token_index);
		longjmp(jmp_buffer, 1);
	}
}

static size_t parse_expr(size_t *i) {
	expr expr = {0};

	token left_token = get_token(*i);
	(*i)++;
	switch (left_token.type) {
		case NUMBER_TOKEN:
		{
			token space_or_newlines_token = get_token(*i);
			if (space_or_newlines_token.type == SPACES_TOKEN && space_or_newlines_token.len == 1) {
				(*i)++;
				// TODO: See https://ts-ast-viewer.com/#code/BQRgBA1GBMCUYCozAMyTAFkcgrOgbLPEsAOzoAcsQA
				struct expr left_expr = {0};
				left_expr.type = LITERAL_EXPR;
				left_expr.literal_expr.str = left_token.str;
				left_expr.literal_expr.len = left_token.len;
				size_t left_expr_index = exprs_size;
				push_expr(left_expr);

				token operator_token = get_token(*i);
				(*i)++;
				if (operator_token.type == PLUS_TOKEN) {
					assert_spaces(*i, 1);
					(*i)++;

					expr.binary_expr.left_expr_index = left_expr_index;
					expr.binary_expr.operator = *operator_token.str;
					expr.binary_expr.right_expr_index = parse_expr(i);
				}
			} else if (space_or_newlines_token.type == NEWLINES_TOKEN) {
				expr.type = LITERAL_EXPR;
				expr.literal_expr.str = left_token.str;
				expr.literal_expr.len = left_token.len;
			}

			break;
		}
		default:
			snprintf(error_msg, sizeof(error_msg), "Expected an expression token, but got token type %s at token index %zu", get_token_type_str[left_token.type], *i - 1);
			longjmp(jmp_buffer, 1);
	}

	push_expr(expr);
	return exprs_size - 1;
}

static void parse_statements(size_t *i, size_t *body_statements_offset, size_t *body_statement_count, size_t indents) {
	(*i)++;
	skip_any_comment(i);

	assert_1_newline(*i);
	(*i)++;

	// This local array is necessary due to the depth-first recursion we're doing
	statement local_statements[MAX_STATEMENTS_PER_STACK_FRAME];
	size_t local_statements_size = 0;

	while (true) {
		token token = get_token(*i);
		if (token.type != SPACES_TOKEN || token.len != indents * SPACES_PER_INDENT) {
			break;
		}
		(*i)++;

		// TODO: Turn this into parse_statement()
		token = get_token(*i);
		(*i)++;
		statement statement;
		switch (token.type) {
			case TEXT_TOKEN:
				statement.type = VARIABLE_STATEMENT;
				break;
			case IF_TOKEN:
				statement.type = IF_STATEMENT;
				break;
			case RETURN_TOKEN:
				statement.type = RETURN_STATEMENT;
				assert_spaces(*i, 1);
				(*i)++;
				// TODO: How do I want to store there being no return value in .return_statement?
				statement.return_statement.value_expr_index = parse_expr(i);
				break;
			case LOOP_TOKEN:
				statement.type = LOOP_STATEMENT;
				statement.loop_statement.body_statement_count = 0;
				assert_spaces(*i, 1);
				(*i)++;
				parse_statements(i, &statement.loop_statement.body_statements_offset, &statement.loop_statement.body_statement_count, indents + 1);
				break;
			case BREAK_TOKEN:
				statement.type = BREAK_STATEMENT;
				break;
			case CONTINUE_TOKEN:
				statement.type = CONTINUE_STATEMENT;
				break;
			default:
				snprintf(error_msg, sizeof(error_msg), "Expected a statement token, but got token type %s at token index %zu", get_token_type_str[token.type], *i);
				longjmp(jmp_buffer, 1);
		}

		// Make sure there's enough room to push statement
		if (local_statements_size + 1 > MAX_STATEMENTS_PER_STACK_FRAME) {
			snprintf(error_msg, sizeof(error_msg), "There are more than %d statements in one of the grug file's stack frames, exceeding MAX_STATEMENTS_PER_STACK_FRAME!", MAX_STATEMENTS_PER_STACK_FRAME);
			longjmp(jmp_buffer, 1);
		}
		local_statements[local_statements_size++] = statement;
		(*body_statement_count)++;
		skip_any_comment(i);

		assert_1_newline(*i);
		(*i)++;
	}

	*body_statements_offset = statements_size;
	for (size_t i = 0; i < local_statements_size; i++) {
		push_statement(local_statements[i]);
	}

	if (indents > 1) {
		assert_spaces(*i, (indents - 1) * SPACES_PER_INDENT);
		(*i)++;
	}

	assert_token_type(*i, CLOSE_BRACE_TOKEN);
	(*i)++;
	skip_any_comment(i);
}

static void push_argument(argument argument) {
	// Make sure there's enough room to push argument
	if (arguments_size + 1 > MAX_ARGUMENTS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d arguments in the grug file, exceeding MAX_ARGUMENTS_IN_FILE!", MAX_ARGUMENTS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	arguments[arguments_size++] = argument;
}

static void parse_on_or_helper_fn_arguments(size_t *i, size_t *arguments_offset, size_t *argument_count) {
	token token = get_token(*i);
	argument argument = {.name = token.str, .name_len = token.len};
	(*i)++;

	assert_token_type(*i, COLON_TOKEN);
	(*i)++;

	assert_spaces(*i, 1);
	(*i)++;

	token = get_token(*i);
	assert_token_type(*i, TEXT_TOKEN);
	argument.type = token.str;
	argument.type_len = token.len;
	*arguments_offset = arguments_size;
	push_argument(argument);
	(*argument_count)++;
	(*i)++;

	// The second, third, etc. arguments all must have a comma before them
	while (true)
	{
		token = get_token(*i);
		if (token.type != COMMA_TOKEN) {
			break;
		}
		(*i)++;

		assert_spaces(*i, 1);
		(*i)++;

		token = get_token(*i);
		assert_token_type(*i, TEXT_TOKEN);
		struct argument argument = {.name = token.str, .name_len = token.len};
		(*i)++;

		assert_token_type(*i, COLON_TOKEN);
		(*i)++;

		assert_spaces(*i, 1);
		(*i)++;

		token = get_token(*i);
		assert_token_type(*i, TEXT_TOKEN);
		argument.type = token.str;
		argument.type_len = token.len;
		push_argument(argument);
		(*argument_count)++;
		(*i)++;
	}
}

static void parse_on_fn(size_t *i) {
	on_fn fn = {0};

	// Parse the function's signature
	token token = get_token(*i);
	fn.fn_name = token.str;
	fn.fn_name_len = token.len;
	(*i)++;

	assert_token_type(*i, OPEN_PARENTHESIS_TOKEN);
	(*i)++;

	// The first argument must not have a comma before it
	token = get_token(*i);
	if (token.type == TEXT_TOKEN) {
		parse_on_or_helper_fn_arguments(i, &fn.arguments_offset, &fn.argument_count);
	}

	assert_token_type(*i, CLOSE_PARENTHESIS_TOKEN);
	(*i)++;

	assert_spaces(*i, 1);
	(*i)++;

	assert_token_type(*i, OPEN_BRACE_TOKEN);
	parse_statements(i, &fn.body_statements_offset, &fn.body_statement_count, 1);

	push_on_fn(fn);
}

static void push_field(field field) {
	// Make sure there's enough room to push field
	if (fields_size + 1 > MAX_FIELDS_IN_FILE) {
		snprintf(error_msg, sizeof(error_msg), "There are more than %d fields in the grug file, exceeding MAX_FIELDS_IN_FILE!", MAX_FIELDS_IN_FILE);
		longjmp(jmp_buffer, 1);
	}
	fields[fields_size++] = field;
}

static compound_literal parse_compound_literal(size_t *i, size_t indents) {
	(*i)++;
	skip_any_comment(i);

	compound_literal compound_literal = {.fields_offset = fields_size};

	assert_1_newline(*i);
	(*i)++;

	size_t expected_spaces = (indents + 1) * SPACES_PER_INDENT;

	// Parse any other fields
	while (true) {
		token token = get_token(*i);
		if (token.type != SPACES_TOKEN || token.len != expected_spaces) {
			break;
		}
		(*i)++;

		token = get_token(*i);
		assert_token_type(*i, FIELD_NAME_TOKEN);
		field field = {.key = token.str, .key_len = token.len};
		(*i)++;

		assert_spaces(*i, 1);
		(*i)++;

		assert_token_type(*i, ASSIGNMENT_TOKEN);
		(*i)++;

		assert_spaces(*i, 1);
		(*i)++;

		token = get_token(*i);
		if (token.type != STRING_TOKEN && token.type != NUMBER_TOKEN) {
			snprintf(error_msg, sizeof(error_msg), "Expected token type STRING_TOKEN or NUMBER_TOKEN, but got %s at token index %zu", get_token_type_str[token.type], *i);
			longjmp(jmp_buffer, 1);
		}
		field.value = token.str;
		field.value_len = token.len;
		push_field(field);
		compound_literal.field_count++;
		(*i)++;

		assert_token_type(*i, COMMA_TOKEN);
		(*i)++;
		skip_any_comment(i);

		assert_1_newline(*i);
		(*i)++;
	}

	if (compound_literal.field_count == 0) {
		snprintf(error_msg, sizeof(error_msg), "Expected at least one field in the compound literal near token index %zu", *i);
		longjmp(jmp_buffer, 1);
	}

	// Close the compound literal
	assert_spaces(*i, indents * SPACES_PER_INDENT);
	(*i)++;

	assert_token_type(*i, CLOSE_BRACE_TOKEN);
	(*i)++;
	skip_any_comment(i);

	assert_1_newline(*i);
	(*i)++;

	return compound_literal;
}

static void parse_define_fn(size_t *i) {
	// Parse the function's signature
	token token = get_token(*i);
	define_fn.fn_name = token.str;
	define_fn.fn_name_len = token.len;
	(*i)++;

	assert_token_type(*i, OPEN_PARENTHESIS_TOKEN);
	(*i)++;

	assert_token_type(*i, CLOSE_PARENTHESIS_TOKEN);
	(*i)++;

	assert_spaces(*i, 1);
	(*i)++;

	token = get_token(*i);
	assert_token_type(*i, TEXT_TOKEN);
	define_fn.return_type = token.str;
	define_fn.return_type_len = token.len;
	(*i)++;

	assert_spaces(*i, 1);
	(*i)++;

	assert_token_type(*i, OPEN_BRACE_TOKEN);
	(*i)++;
	skip_any_comment(i);

	assert_1_newline(*i);
	(*i)++;

	// Parse the body of the function
	assert_spaces(*i, SPACES_PER_INDENT);
	(*i)++;

	assert_token_type(*i, RETURN_TOKEN);
	(*i)++;

	assert_spaces(*i, 1);
	(*i)++;

	assert_token_type(*i, OPEN_BRACE_TOKEN);
	define_fn.returned_compound_literal = parse_compound_literal(i, 1);

	// Close the function
	assert_token_type(*i, CLOSE_BRACE_TOKEN);
	(*i)++;
	skip_any_comment(i);
}

static bool starts_with(char *a, char *b) {
	return strncmp(a, b, strlen(b)) == 0;
}

static void parse() {
	size_t i = 0;
	while (i < tokens_size) {
		token token = get_token(i);
		int type = token.type;

		if (       type == TEXT_TOKEN && starts_with(token.str, "define_")) {
			parse_define_fn(&i);
		} else if (type == TEXT_TOKEN && starts_with(token.str, "on_")) {
			parse_on_fn(&i);
		} else if (type == TEXT_TOKEN) {
			parse_helper_fn(&i);
		} else if (type == COMMENT_TOKEN) {
			i++;
		} else if (type == NEWLINES_TOKEN) {
			i++;
		} else {
			snprintf(error_msg, sizeof(error_msg), "Unexpected token '%.*s' at token index %zu in parse()", (int)token.len, token.str, i);
			longjmp(jmp_buffer, 1);
		}
	}
}

static void reset() {
	tokens_size = 0;
	fields_size = 0;
	exprs_size = 0;
	statements_size = 0;
	arguments_size = 0;
	helper_fns_size = 0;
	on_fns_size = 0;
}

static char *read_file(char *path) {
	FILE *f = fopen(path, "rb");
	if (!f) {
        snprintf(error_msg, sizeof(error_msg), "fopen");
		longjmp(jmp_buffer, 1);
	}

	if (fseek(f, 0, SEEK_END)) {
        snprintf(error_msg, sizeof(error_msg), "fseek");
		longjmp(jmp_buffer, 1);
	}

	long count = ftell(f);
	if (count == -1) {
        snprintf(error_msg, sizeof(error_msg), "ftell");
		longjmp(jmp_buffer, 1);
	}

	rewind(f);

	char *text = malloc(count + 1);
	if (!text) {
		snprintf(error_msg, sizeof(error_msg), "malloc");
		longjmp(jmp_buffer, 1);
	}

	ssize_t bytes_read = fread(text, 1, count, f);
	if (bytes_read != count) {
        snprintf(error_msg, sizeof(error_msg), "fread");
		longjmp(jmp_buffer, 1);
	}

	text[count] = '\0';

	return text;
}

void run() {
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

	free(grug_text);
}

void error_handler(char *error_msg) {
	fprintf(stderr, "%s\n", error_msg);
	exit(EXIT_FAILURE);
}

// clang testing.c -Wall -Wextra -Werror -Wpedantic -Wfatal-errors -fsanitize=address,undefined -g && ./a.out
int main() {
	grug_error_handler = error_handler;

	if (setjmp(jmp_buffer)) {
		if (grug_error_handler == NULL) {
			fprintf(stderr, "An error occurred, but the game forgot to do `grug_error_handler = your_error_handler_function;`, so grug wasn't able to execute `grug_error_handler(error_msg);`\n");
			exit(EXIT_FAILURE);
		}

		grug_error_handler(error_msg);
	}

	run();

	// TODO: REMOVE
	(void)exprs;
	(void)helper_fns;
}
