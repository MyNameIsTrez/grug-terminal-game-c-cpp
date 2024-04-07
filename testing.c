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
#define MAX_CALL_ARGUMENTS_PER_STACK_FRAME 69
#define MAX_STATEMENTS_PER_STACK_FRAME 1337

#define GRUG_ERROR(...) {\
	snprintf(error_msg, sizeof(error_msg), __VA_ARGS__);\
	error_line = __LINE__;\
	longjmp(jmp_buffer, 1);\
}

static char error_msg[420];
static int error_line;
jmp_buf jmp_buffer;

typedef void (*grug_error_handler_fn)(char *error_msg);
grug_error_handler_fn grug_error_handler;

//// TOKENIZATION

typedef struct token token;

enum token_type {
	OPEN_PARENTHESIS_TOKEN,
	CLOSE_PARENTHESIS_TOKEN,
	OPEN_BRACE_TOKEN,
	CLOSE_BRACE_TOKEN,
	PLUS_TOKEN,
	MINUS_TOKEN,
	MULTIPLICATION_TOKEN,
	DIVISION_TOKEN,
	REMAINDER_TOKEN,
	COMMA_TOKEN,
	COLON_TOKEN,
	EQUALS_TOKEN,
	NOT_EQUALS_TOKEN,
	ASSIGNMENT_TOKEN,
	GREATER_OR_EQUAL_TOKEN,
	GREATER_TOKEN,
	LESS_OR_EQUAL_TOKEN,
	LESS_TOKEN,
	NOT_TOKEN,
	TRUE_TOKEN,
	FALSE_TOKEN,
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
	WORD_TOKEN,
	NUMBER_TOKEN,
	COMMENT_TOKEN,
};

struct token {
	enum token_type type;
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
	[MULTIPLICATION_TOKEN] = "MULTIPLICATION_TOKEN",
	[DIVISION_TOKEN] = "DIVISION_TOKEN",
	[REMAINDER_TOKEN] = "REMAINDER_TOKEN",
	[COMMA_TOKEN] = "COMMA_TOKEN",
	[COLON_TOKEN] = "COLON_TOKEN",
	[EQUALS_TOKEN] = "EQUALS_TOKEN",
	[NOT_EQUALS_TOKEN] = "NOT_EQUALS_TOKEN",
	[ASSIGNMENT_TOKEN] = "ASSIGNMENT_TOKEN",
	[GREATER_OR_EQUAL_TOKEN] = "GREATER_OR_EQUAL_TOKEN",
	[GREATER_TOKEN] = "GREATER_TOKEN",
	[LESS_OR_EQUAL_TOKEN] = "LESS_OR_EQUAL_TOKEN",
	[LESS_TOKEN] = "LESS_TOKEN",
	[NOT_TOKEN] = "NOT_TOKEN",
	[TRUE_TOKEN] = "TRUE_TOKEN",
	[FALSE_TOKEN] = "FALSE_TOKEN",
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
	[WORD_TOKEN] = "WORD_TOKEN",
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

static token peek_token(size_t token_index) {
	if (token_index >= tokens_size) {
		GRUG_ERROR("token_index %zu was out of bounds in peek_token()", token_index);
	}
	return tokens[token_index];
}

static token consume_token(size_t *token_index_ptr) {
	return peek_token((*token_index_ptr)++);
}

static void print_tokens() {
	size_t longest_token_type_len = 0;
	for (size_t i = 0; i < tokens_size; i++) {
		token token = peek_token(i);
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
		token token = peek_token(i);

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

	printf("\n");
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
		GRUG_ERROR("There are more than %d tokens in the grug file, exceeding MAX_TOKENS_IN_FILE", MAX_TOKENS_IN_FILE);
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
		} else if (grug_text[i] == '*') {
			push_token((token){.type=MULTIPLICATION_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '/') {
			push_token((token){.type=DIVISION_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '%') {
			push_token((token){.type=REMAINDER_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ',') {
			push_token((token){.type=COMMA_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == ':') {
			push_token((token){.type=COLON_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '=' && grug_text[i + 1] == '=') {
			push_token((token){.type=EQUALS_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '!' && grug_text[i + 1] == '=') {
			push_token((token){.type=NOT_EQUALS_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '=') {
			push_token((token){.type=ASSIGNMENT_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '>' && grug_text[i + 1] == '=') {
			push_token((token){.type=GREATER_OR_EQUAL_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '>') {
			push_token((token){.type=GREATER_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i] == '<' && grug_text[i + 1] == '=') {
			push_token((token){.type=LESS_OR_EQUAL_TOKEN, .str=grug_text+i, .len=2});
			i += 2;
		} else if (grug_text[i] == '<') {
			push_token((token){.type=LESS_TOKEN, .str=grug_text+i, .len=1});
			i += 1;
		} else if (grug_text[i + 0] == 'n' && grug_text[i + 1] == 'o' && grug_text[i + 2] == 't' && grug_text[i + 3] == ' ') {
			push_token((token){.type=NOT_TOKEN, .str=grug_text+i, .len=3});
			i += 3;
		} else if (grug_text[i + 0] == 't' && grug_text[i + 1] == 'r' && grug_text[i + 2] == 'u' && grug_text[i + 3] == 'e' && grug_text[i + 4] == ' ') {
			push_token((token){.type=TRUE_TOKEN, .str=grug_text+i, .len=4});
			i += 4;
		} else if (grug_text[i + 0] == 'f' && grug_text[i + 1] == 'a' && grug_text[i + 2] == 'l' && grug_text[i + 3] == 's' && grug_text[i + 4] == 'e' && grug_text[i + 5] == ' ') {
			push_token((token){.type=FALSE_TOKEN, .str=grug_text+i, .len=5});
			i += 5;
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
			token token = {.type=WORD_TOKEN, .str=grug_text+i};

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

					GRUG_ERROR("Unexpected unprintable character '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[i]) ? 2 : 1, get_escaped_char(&grug_text[i]), i + 1);
				}
			}

			token.len = i - (token.str - grug_text);
			push_token(token);
		} else {
			GRUG_ERROR("Unrecognized character '%.*s' at character %zu of the grug text file", is_escaped_char(grug_text[i]) ? 2 : 1, get_escaped_char(&grug_text[i]), i + 1);
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
typedef struct parenthesized_expr parenthesized_expr;
typedef struct expr expr;
typedef struct variable_statement variable_statement;
typedef struct call_statement call_statement;
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
	enum token_type operator;
	size_t expr_index;
};

struct binary_expr {
	size_t left_expr_index;
	enum token_type operator;
	size_t right_expr_index;
};

struct call_expr {
	char *fn_name;
	size_t fn_name_len;
	size_t arguments_exprs_offset;
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

struct parenthesized_expr {
	size_t expr_index;
};

struct expr {
	enum {
		TRUE_EXPR,
		FALSE_EXPR,
		STRING_EXPR,
		NUMBER_EXPR,
		UNARY_EXPR,
		BINARY_EXPR,
		CALL_EXPR,
		PARENTHESIZED_EXPR,
	} type;
	union {
		literal_expr literal_expr;
		unary_expr unary_expr;
		binary_expr binary_expr;
		call_expr call_expr;
		parenthesized_expr parenthesized_expr;
	};
};
static char *get_expr_type_str[] = {
	[TRUE_EXPR] = "TRUE_EXPR",
	[FALSE_EXPR] = "FALSE_EXPR",
	[STRING_EXPR] = "STRING_EXPR",
	[NUMBER_EXPR] = "NUMBER_EXPR",
	[UNARY_EXPR] = "UNARY_EXPR",
	[BINARY_EXPR] = "BINARY_EXPR",
	[CALL_EXPR] = "CALL_EXPR",
	[PARENTHESIZED_EXPR] = "PARENTHESIZED_EXPR",
};
static expr exprs[MAX_EXPRS_IN_FILE];
static size_t exprs_size;

struct variable_statement {
	char *name;
	size_t name_len;
	char *type;
	size_t type_len;
	bool has_type;
	size_t assignment_expr_index;
	bool has_assignment;
};

struct call_statement {
	size_t expr_index;
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
	bool has_value;
};

struct loop_statement {
	size_t body_statements_offset;
	size_t body_statement_count;
};

struct statement {
	enum {
		VARIABLE_STATEMENT,
		CALL_STATEMENT,
		IF_STATEMENT,
		RETURN_STATEMENT,
		LOOP_STATEMENT,
		BREAK_STATEMENT,
		CONTINUE_STATEMENT,
	} type;
	union {
		variable_statement variable_statement;
		call_statement call_statement;
		if_statement if_statement;
		return_statement return_statement;
		loop_statement loop_statement;
	};
};
static char *get_statement_type_str[] = {
	[VARIABLE_STATEMENT] = "VARIABLE_STATEMENT",
	[CALL_STATEMENT] = "CALL_STATEMENT",
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

static char *serialize_to_c() {
	char *c_text;

	c_text = "";

	return c_text;
}

static void print_helper_fns() {
}

static void print_expr(expr expr);

static void print_parenthesized_expr(parenthesized_expr parenthesized_expr) {
	printf("\"expr\": {\n");
	print_expr(exprs[parenthesized_expr.expr_index]);
	printf("},\n");
}

static void print_call_expr(call_expr call_expr) {
	printf("\"fn_name\": \"%.*s\",\n", (int)call_expr.fn_name_len, call_expr.fn_name);

	printf("\"arguments\": [\n");
	for (size_t argument_index = 0; argument_index < call_expr.argument_count; argument_index++) {
		printf("{\n");
		print_expr(exprs[call_expr.arguments_exprs_offset + argument_index]);
		printf("},\n");
	}
	printf("],\n");
}

static void print_binary_expr(binary_expr binary_expr) {
	printf("\"left_expr\": {\n");
	print_expr(exprs[binary_expr.left_expr_index]);
	printf("},\n");
	printf("\"operator\": \"%s\",\n", get_token_type_str[binary_expr.operator]);
	printf("\"right_expr\": {\n");
	print_expr(exprs[binary_expr.right_expr_index]);
	printf("},\n");
}

static void print_expr(expr expr) {
	printf("\"type\": \"%s\",\n", get_expr_type_str[expr.type]);

	switch (expr.type) {
		case TRUE_EXPR:
		case FALSE_EXPR:
			break;
		case STRING_EXPR:
		case NUMBER_EXPR:
			printf("\"str\": \"%.*s\",\n", (int)expr.literal_expr.len, expr.literal_expr.str);
			break;
		case UNARY_EXPR:
			printf("\"operator\": \"%s\",\n", get_token_type_str[expr.unary_expr.operator]);
			printf("\"expr\": {\n");
			print_expr(exprs[expr.unary_expr.expr_index]);
			printf("},\n");
			break;
		case BINARY_EXPR:
			print_binary_expr(expr.binary_expr);
			break;
		case CALL_EXPR:
			print_call_expr(expr.call_expr);
			break;
		case PARENTHESIZED_EXPR:
			print_parenthesized_expr(expr.parenthesized_expr);
			break;
	}
}

static void print_statements(size_t statements_offset, size_t statement_count) {
	for (size_t statement_index = 0; statement_index < statement_count; statement_index++) {
		printf("{\n");

		statement st = statements[statements_offset + statement_index];

		printf("\"type\": \"%s\",\n", get_statement_type_str[st.type]);

		switch (st.type) {
			case VARIABLE_STATEMENT:
				printf("\"variable_name\": \"%.*s\",\n", (int)st.variable_statement.name_len, st.variable_statement.name);

				if (st.variable_statement.has_type) {
					printf("\"variable_type\": \"%.*s\",\n", (int)st.variable_statement.type_len, st.variable_statement.type);
				}

				if (st.variable_statement.has_assignment) {
					printf("\"assignment\": {\n");
					print_expr(exprs[st.variable_statement.assignment_expr_index]);
					printf("},\n");
				}

				break;
			case CALL_STATEMENT:
				print_call_expr(exprs[st.call_statement.expr_index].call_expr);
				break;
			case IF_STATEMENT:
				printf("\"condition\": {\n");
				print_expr(st.if_statement.condition);
				printf("},\n");

				printf("\"if_statements\": [\n");
				print_statements(st.if_statement.if_body_statements_offset, st.if_statement.if_body_statement_count);
				printf("],\n");

				if (st.if_statement.else_body_statement_count > 0) {
					printf("\"else_statements\": [\n");
					print_statements(st.if_statement.else_body_statements_offset, st.if_statement.else_body_statement_count);
					printf("],\n");
				}

				break;
			case RETURN_STATEMENT:
				if (st.return_statement.has_value) {
					expr return_expr = exprs[st.return_statement.value_expr_index];
					printf("\"expr\": {\n");
					print_expr(return_expr);
					printf("},\n");
				}
				break;
			case LOOP_STATEMENT:
				printf("\"statements\": [\n");
				print_statements(st.loop_statement.body_statements_offset, st.loop_statement.body_statement_count);
				printf("],\n");
				break;
			case BREAK_STATEMENT:
				break;
			case CONTINUE_STATEMENT:
				break;
		}

		printf("},\n");
	}
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

		printf("\"statements\": [\n");
		print_statements(fn.body_statements_offset, fn.body_statement_count);
		printf("],\n");

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
		GRUG_ERROR("There are more than %d on_fns in the grug file, exceeding MAX_ON_FNS_IN_FILE", MAX_ON_FNS_IN_FILE);
	}
	on_fns[on_fns_size++] = on_fn;
}

static size_t push_statement(statement statement) {
	// Make sure there's enough room to push statement
	if (statements_size + 1 > MAX_STATEMENTS_IN_FILE) {
		GRUG_ERROR("There are more than %d statements in the grug file, exceeding MAX_STATEMENTS_IN_FILE", MAX_STATEMENTS_IN_FILE);
	}
	statements[statements_size] = statement;
	return statements_size++;
}

static size_t push_expr(expr expr) {
	// Make sure there's enough room to push expr
	if (exprs_size + 1 > MAX_EXPRS_IN_FILE) {
		GRUG_ERROR("There are more than %d exprs in the grug file, exceeding MAX_EXPRS_IN_FILE", MAX_EXPRS_IN_FILE);
	}
	exprs[exprs_size] = expr;
	return exprs_size++;
}

static void potentially_skip_comment(size_t *i) {
	token token = peek_token(*i);
	if (token.type == COMMENT_TOKEN) {
		(*i)++;
	}
}

static void assert_token_type(size_t token_index, unsigned int expected_type) {
	token token = peek_token(token_index);
	if (token.type != expected_type) {
		GRUG_ERROR("Expected token type %s, but got %s at token index %zu", get_token_type_str[expected_type], get_token_type_str[token.type], token_index);
	}
}

static void consume_token_type(size_t *token_index_ptr, unsigned int expected_type) {
	assert_token_type((*token_index_ptr)++, expected_type);
}

static void consume_1_newline(size_t *token_index_ptr) {
	assert_token_type(*token_index_ptr, NEWLINES_TOKEN);

	token token = peek_token(*token_index_ptr);
	if (token.len != 1) {
		GRUG_ERROR("Expected 1 newline, but got %zu at token index %zu", token.len, *token_index_ptr);
	}

	(*token_index_ptr)++;
}

static expr parse_expression(size_t *i);

static void parse_fn_call(size_t *i, size_t *arguments_exprs_offset, size_t *argument_count) {
	*argument_count = 0;

	token token = peek_token(*i);
	if (token.type != CLOSE_PARENTHESIS_TOKEN) {
		struct expr local_call_arguments[MAX_CALL_ARGUMENTS_PER_STACK_FRAME];

		while (true) {
			struct expr call_argument = parse_expression(i);

			// Make sure there's enough room to push call_argument
			if (*argument_count + 1 > MAX_CALL_ARGUMENTS_PER_STACK_FRAME) {
				GRUG_ERROR("There are more than %d arguments to a function call in one of the grug file's stack frames, exceeding MAX_CALL_ARGUMENTS_PER_STACK_FRAME", MAX_CALL_ARGUMENTS_PER_STACK_FRAME);
			}
			local_call_arguments[(*argument_count)++] = call_argument;

			token = consume_token(i);
			if (token.type != COMMA_TOKEN) {
				break;
			}
		}

		*arguments_exprs_offset = exprs_size;
		for (size_t i = 0; i < *argument_count; i++) {
			push_expr(local_call_arguments[i]);
		}
	} else {
		(*i)++;
	}
}

static expr parse_primary(size_t *i) {
	token token = peek_token(*i);

	expr expr = {0};

	switch (token.type) {
		case TRUE_TOKEN:
			(*i)++;
			expr.type = TRUE_EXPR;
			return expr;
		case FALSE_TOKEN:
			(*i)++;
			expr.type = FALSE_EXPR;
			return expr;
		case STRING_TOKEN:
			(*i)++;
			expr.type = STRING_EXPR;
			expr.literal_expr.str = token.str;
			expr.literal_expr.len = token.len;
			return expr;
		case NUMBER_TOKEN:
			(*i)++;
			expr.type = NUMBER_EXPR;
			expr.literal_expr.str = token.str;
			expr.literal_expr.len = token.len;
			return expr;
		case OPEN_PARENTHESIS_TOKEN:
			(*i)++;
			expr.type = PARENTHESIZED_EXPR;
			expr.parenthesized_expr.expr_index = push_expr(parse_expression(i));
			consume_token_type(i, CLOSE_PARENTHESIS_TOKEN);
			return expr;
		default:
			GRUG_ERROR("Expected a primary expression token, but got token type %s at token index %zu", get_token_type_str[token.type], *i);
	}
}

static expr parse_unary(size_t *i) {
	token token = peek_token(*i);
	if (token.type == NOT_TOKEN
	 || token.type == MINUS_TOKEN) {
		(*i)++;
		expr expr = {0};

		expr.unary_expr.operator = token.type;
		expr.unary_expr.expr_index = push_expr(parse_unary(i));
		expr.type = UNARY_EXPR;
		
		return expr;
	 }

	return parse_primary(i);
}

static expr parse_factor(size_t *i) {
	expr expr = parse_unary(i);

	while (true) {
		token token = peek_token(*i);
		if (token.type != MULTIPLICATION_TOKEN
	     && token.type != DIVISION_TOKEN) {
			break;
		}
		(*i)++;
		expr.binary_expr.left_expr_index = push_expr(expr);
		expr.binary_expr.operator = token.type;
		expr.binary_expr.right_expr_index = push_expr(parse_unary(i));
		expr.type = BINARY_EXPR;
	}

	return expr;
}

static expr parse_term(size_t *i) {
	expr expr = parse_factor(i);

	while (true) {
		token token = peek_token(*i);
		if (token.type != PLUS_TOKEN
	     && token.type != MINUS_TOKEN) {
			break;
		}
		(*i)++;
		expr.binary_expr.left_expr_index = push_expr(expr);
		expr.binary_expr.operator = token.type;
		expr.binary_expr.right_expr_index = push_expr(parse_factor(i));
		expr.type = BINARY_EXPR;
	}

	return expr;
}

static expr parse_comparison(size_t *i) {
	expr expr = parse_term(i);

	while (true) {
		token token = peek_token(*i);
		if (token.type != GREATER_OR_EQUAL_TOKEN
	     && token.type != GREATER_TOKEN
		 && token.type != LESS_OR_EQUAL_TOKEN
		 && token.type != LESS_TOKEN) {
			break;
		}
		(*i)++;
		expr.binary_expr.left_expr_index = push_expr(expr);
		expr.binary_expr.operator = token.type;
		expr.binary_expr.right_expr_index = push_expr(parse_term(i));
		expr.type = BINARY_EXPR;
	}

	return expr;
}

static expr parse_equality(size_t *i) {
	expr expr = parse_comparison(i);

	while (true) {
		token token = peek_token(*i);
		if (token.type != EQUALS_TOKEN
	     && token.type != NOT_EQUALS_TOKEN) {
			break;
		}
		(*i)++;
		expr.binary_expr.left_expr_index = push_expr(expr);
		expr.binary_expr.operator = token.type;
		expr.binary_expr.right_expr_index = push_expr(parse_comparison(i));
		expr.type = BINARY_EXPR;
	}

	return expr;
}

// Recursive descent parsing inspired by the book Crafting Interpreters:
// https://craftinginterpreters.com/parsing-expressions.html#recursive-descent-parsing
static expr parse_expression(size_t *i) {
	return parse_equality(i);
}

static void parse_statements(size_t *i, size_t *body_statements_offset, size_t *body_statement_count);

static statement parse_if_statement(size_t *i) {
	statement statement = {0};
	statement.type = IF_STATEMENT;
	statement.if_statement.condition = parse_expression(i);

	parse_statements(i, &statement.if_statement.if_body_statements_offset, &statement.if_statement.if_body_statement_count);

	if (peek_token(*i).type == ELSE_TOKEN) {
		(*i)++;

		if (peek_token(*i).type == IF_TOKEN) {
			(*i)++;

			statement.if_statement.else_body_statement_count = 1;

			struct statement else_if_statement = parse_if_statement(i);
			statement.if_statement.else_body_statements_offset = push_statement(else_if_statement);
		} else {
			parse_statements(i, &statement.if_statement.else_body_statements_offset, &statement.if_statement.else_body_statement_count);
		}
	}

	return statement;
}

static variable_statement parse_variable_statement(size_t *i, token name_token) {
	variable_statement variable_statement = {0};
	variable_statement.name = name_token.str;
	variable_statement.name_len = name_token.len;

	token token = peek_token(*i);
	if (token.type == COLON_TOKEN) {
		(*i)++;

		struct token type_token = consume_token(i);
		if (type_token.type == WORD_TOKEN) {
			variable_statement.has_type = true;
			variable_statement.type = type_token.str;
			variable_statement.type_len = type_token.len;
		} else {
			GRUG_ERROR("Expected a word token after the colon at token index %zu", *i - 3);
		}
	}

	token = peek_token(*i);
	if (token.type == ASSIGNMENT_TOKEN) {
		(*i)++;

		variable_statement.has_assignment = true;

		expr expr = parse_expression(i);
		variable_statement.assignment_expr_index = push_expr(expr);
	}

	return variable_statement;
}

static statement parse_statement(size_t *i) {
	token switch_token = consume_token(i);

	statement statement = {0};
	switch (switch_token.type) {
		case WORD_TOKEN: {
			token token = peek_token(*i);
			if (token.type == OPEN_PARENTHESIS_TOKEN) {
				(*i)++;

				statement.type = CALL_STATEMENT;

				expr expr = {0};
				expr.type = CALL_EXPR;
				expr.call_expr.fn_name = switch_token.str;
				expr.call_expr.fn_name_len = switch_token.len;

				parse_fn_call(i, &expr.call_expr.arguments_exprs_offset, &expr.call_expr.argument_count);

				statement.call_statement.expr_index = push_expr(expr);
			} else if (token.type == COLON_TOKEN || (*i + 1 < tokens_size && peek_token(*i + 1).type == ASSIGNMENT_TOKEN)) {
				statement.type = VARIABLE_STATEMENT;
				statement.variable_statement = parse_variable_statement(i, switch_token);
			} else {
				GRUG_ERROR("Expected '(' or ':' or ' =' after the word '%.*s' at token index %zu", (int)switch_token.len, switch_token.str, *i - 2);
			}

			break;
		}
		case IF_TOKEN:
			statement = parse_if_statement(i);
			break;
		case RETURN_TOKEN:
			statement.type = RETURN_STATEMENT;

			token token = peek_token(*i);
			if (token.type == NEWLINES_TOKEN) {
				statement.return_statement.has_value = false;
			} else {
				statement.return_statement.has_value = true;
				expr value_expr = parse_expression(i);
				statement.return_statement.value_expr_index = push_expr(value_expr);
			}

			break;
		case LOOP_TOKEN:
			statement.type = LOOP_STATEMENT;
			parse_statements(i, &statement.loop_statement.body_statements_offset, &statement.loop_statement.body_statement_count);
			break;
		case BREAK_TOKEN:
			statement.type = BREAK_STATEMENT;
			break;
		case CONTINUE_TOKEN:
			statement.type = CONTINUE_STATEMENT;
			break;
		default:
			GRUG_ERROR("Expected a statement token, but got token type %s at token index %zu", get_token_type_str[token.type], *i);
	}

	return statement;
}

static void parse_statements(size_t *i, size_t *body_statements_offset, size_t *body_statement_count) {
	consume_token_type(i, OPEN_BRACE_TOKEN);
	potentially_skip_comment(i);

	consume_1_newline(i);

	// This local array is necessary, cause an IF or LOOP substatement can contain its own statements
	statement local_statements[MAX_STATEMENTS_PER_STACK_FRAME];
	*body_statement_count = 0;

	while (true) {
		token token = peek_token(*i);
		if (token.type == CLOSE_BRACE_TOKEN) {
			break;
		}
		(*i)++;

		if (token.type != COMMENT_TOKEN) {
			statement statement = parse_statement(i);

			// Make sure there's enough room to push statement
			if (*body_statement_count + 1 > MAX_STATEMENTS_PER_STACK_FRAME) {
				GRUG_ERROR("There are more than %d statements in one of the grug file's stack frames, exceeding MAX_STATEMENTS_PER_STACK_FRAME", MAX_STATEMENTS_PER_STACK_FRAME);
			}
			local_statements[(*body_statement_count)++] = statement;
		}
		potentially_skip_comment(i);

		consume_token_type(i, NEWLINES_TOKEN);
	}

	*body_statements_offset = statements_size;
	for (size_t i = 0; i < *body_statement_count; i++) {
		push_statement(local_statements[i]);
	}

	consume_token_type(i, CLOSE_BRACE_TOKEN);

	if (peek_token(*i).type != ELSE_TOKEN) {
		potentially_skip_comment(i);
	}
}

static size_t push_argument(argument argument) {
	// Make sure there's enough room to push argument
	if (arguments_size + 1 > MAX_ARGUMENTS_IN_FILE) {
		GRUG_ERROR("There are more than %d arguments in the grug file, exceeding MAX_ARGUMENTS_IN_FILE", MAX_ARGUMENTS_IN_FILE);
	}
	arguments[arguments_size] = argument;
	return arguments_size++;
}

static void parse_arguments(size_t *i, size_t *arguments_offset, size_t *argument_count) {
	token token = consume_token(i);
	argument argument = {.name = token.str, .name_len = token.len};

	consume_token_type(i, COLON_TOKEN);

	assert_token_type(*i, WORD_TOKEN);
	token = consume_token(i);

	argument.type = token.str;
	argument.type_len = token.len;
	*arguments_offset = push_argument(argument);
	(*argument_count)++;

	// Every argument after the first one starts with a comma
	while (true) {
		token = peek_token(*i);
		if (token.type != COMMA_TOKEN) {
			break;
		}
		(*i)++;

		assert_token_type(*i, WORD_TOKEN);
		token = consume_token(i);
		struct argument argument = {.name = token.str, .name_len = token.len};

		consume_token_type(i, COLON_TOKEN);

		assert_token_type(*i, WORD_TOKEN);
		token = consume_token(i);
		argument.type = token.str;
		argument.type_len = token.len;
		push_argument(argument);
		(*argument_count)++;
	}
}

static void parse_on_fn(size_t *i) {
	on_fn fn = {0};

	token token = consume_token(i);
	fn.fn_name = token.str;
	fn.fn_name_len = token.len;

	consume_token_type(i, OPEN_PARENTHESIS_TOKEN);

	token = peek_token(*i);
	if (token.type == WORD_TOKEN) {
		parse_arguments(i, &fn.arguments_offset, &fn.argument_count);
	}

	consume_token_type(i, CLOSE_PARENTHESIS_TOKEN);

	parse_statements(i, &fn.body_statements_offset, &fn.body_statement_count);

	push_on_fn(fn);
}

static void push_field(field field) {
	// Make sure there's enough room to push field
	if (fields_size + 1 > MAX_FIELDS_IN_FILE) {
		GRUG_ERROR("There are more than %d fields in the grug file, exceeding MAX_FIELDS_IN_FILE", MAX_FIELDS_IN_FILE);
	}
	fields[fields_size++] = field;
}

static compound_literal parse_compound_literal(size_t *i) {
	(*i)++;
	potentially_skip_comment(i);

	compound_literal compound_literal = {.fields_offset = fields_size};

	consume_1_newline(i);

	while (true) {
		token token = peek_token(*i);
		if (token.type == CLOSE_BRACE_TOKEN) {
			break;
		}

		assert_token_type(*i, FIELD_NAME_TOKEN);
		field field = {.key = token.str, .key_len = token.len};
		(*i)++;

		consume_token_type(i, ASSIGNMENT_TOKEN);

		token = peek_token(*i);
		if (token.type != STRING_TOKEN && token.type != NUMBER_TOKEN) {
			GRUG_ERROR("Expected token type STRING_TOKEN or NUMBER_TOKEN, but got %s at token index %zu", get_token_type_str[token.type], *i);
		}
		field.value = token.str;
		field.value_len = token.len;
		push_field(field);
		compound_literal.field_count++;
		(*i)++;

		consume_token_type(i, COMMA_TOKEN);
		potentially_skip_comment(i);

		consume_1_newline(i);
	}

	if (compound_literal.field_count == 0) {
		GRUG_ERROR("Expected at least one field in the compound literal near token index %zu", *i);
	}

	consume_token_type(i, CLOSE_BRACE_TOKEN);
	potentially_skip_comment(i);

	consume_1_newline(i);

	return compound_literal;
}

static void parse_define_fn(size_t *i) {
	// Parse the function's signature
	token token = consume_token(i);
	define_fn.fn_name = token.str;
	define_fn.fn_name_len = token.len;

	consume_token_type(i, OPEN_PARENTHESIS_TOKEN);
	consume_token_type(i, CLOSE_PARENTHESIS_TOKEN);

	assert_token_type(*i, WORD_TOKEN);
	token = consume_token(i);
	define_fn.return_type = token.str;
	define_fn.return_type_len = token.len;

	consume_token_type(i, OPEN_BRACE_TOKEN);
	potentially_skip_comment(i);

	consume_1_newline(i);

	// Parse the body of the function
	consume_token_type(i, RETURN_TOKEN);

	assert_token_type(*i, OPEN_BRACE_TOKEN);
	define_fn.returned_compound_literal = parse_compound_literal(i);

	consume_token_type(i, CLOSE_BRACE_TOKEN);
	potentially_skip_comment(i);
}

static bool starts_with(char *a, char *b) {
	return strncmp(a, b, strlen(b)) == 0;
}

static void parse() {
	size_t i = 0;
	while (i < tokens_size) {
		token token = peek_token(i);
		int type = token.type;

		if (       type == WORD_TOKEN && starts_with(token.str, "define_")) {
			parse_define_fn(&i);
		} else if (type == WORD_TOKEN && starts_with(token.str, "on_")) {
			parse_on_fn(&i);
		} else if (type == WORD_TOKEN) {
			parse_helper_fn(&i);
		} else if (type == COMMENT_TOKEN) {
			i++;
		} else if (type == NEWLINES_TOKEN) {
			i++;
		} else {
			GRUG_ERROR("Unexpected token '%.*s' at token index %zu in parse()", (int)token.len, token.str, i);
		}
	}
}

static void assert_spaces(size_t token_index, size_t expected_spaces) {
	assert_token_type(token_index, SPACES_TOKEN);

	token token = peek_token(token_index);
	if (token.len != expected_spaces) {
		GRUG_ERROR("Expected %zu space%s, but got %zu at token index %zu", expected_spaces, expected_spaces > 1 ? "s" : "", token.len, token_index);
	}
}

// Trims whitespace tokens after verifying that the formatting is correct.
// 1. The whitespace indentation follows the block scope nesting, like in Python.
// 2. There aren't any leading/trailing/missing/extra spaces.
static void verify_and_trim_space_tokens() {
	size_t i = 0;
	size_t new_index = 0;
	int depth = 0;

	while (i < tokens_size) {
		token token = peek_token(i);

		switch (token.type) {
			case OPEN_PARENTHESIS_TOKEN:
			case CLOSE_PARENTHESIS_TOKEN:
			case OPEN_BRACE_TOKEN:
				break;
			case CLOSE_BRACE_TOKEN: {
				depth--;
				if (depth < 0) {
					GRUG_ERROR("Expected a '{' to match the '}' at token index %zu", i + 1);
				}
				if (depth > 0) {
					assert_spaces(i - 1, depth * SPACES_PER_INDENT);
				}
				break;
			}
			case PLUS_TOKEN:
			case MINUS_TOKEN:
			case MULTIPLICATION_TOKEN:
			case DIVISION_TOKEN:
			case REMAINDER_TOKEN:
				break;
			case COMMA_TOKEN: {
				if (i + 1 >= tokens_size) {
					GRUG_ERROR("Expected something after the comma at token index %zu", i);
				}

				struct token next_token = peek_token(i + 1);
				if (next_token.type != NEWLINES_TOKEN && next_token.type != SPACES_TOKEN) {
					GRUG_ERROR("Expected a single newline or space after the comma, but got token type %s at token index %zu", get_token_type_str[next_token.type], i + 1);
				}
				if (next_token.len != 1) {
					GRUG_ERROR("Expected one newline or space, but got several after the comma at token index %zu", i + 1);
				}

				if (next_token.type == SPACES_TOKEN) {
					if (i + 2 >= tokens_size) {
						GRUG_ERROR("Expected text after the comma and space at token index %zu", i);
					}

					next_token = peek_token(i + 2);
					switch (next_token.type) {
						case OPEN_PARENTHESIS_TOKEN:
						case MINUS_TOKEN:
						case STRING_TOKEN:
						case WORD_TOKEN:
						case NUMBER_TOKEN:
							break;
						default:
							GRUG_ERROR("Unexpected token type %s after the comma and space, at token index %zu", get_token_type_str[next_token.type], i + 2);
					}
				}
				break;
			}
			case COLON_TOKEN:
			case EQUALS_TOKEN:
			case NOT_EQUALS_TOKEN:
			case ASSIGNMENT_TOKEN:
			case GREATER_OR_EQUAL_TOKEN:
			case GREATER_TOKEN:
			case LESS_OR_EQUAL_TOKEN:
			case LESS_TOKEN:
			case NOT_TOKEN:
			case TRUE_TOKEN:
			case FALSE_TOKEN:
			case IF_TOKEN:
			case ELSE_TOKEN:
			case LOOP_TOKEN:
			case BREAK_TOKEN:
			case RETURN_TOKEN:
			case CONTINUE_TOKEN:
				break;
			case SPACES_TOKEN: {
				if (i + 1 >= tokens_size) {
					GRUG_ERROR("Expected another token after the space at token index %zu", i);
				}

				struct token next_token = peek_token(i + 1);
				switch (next_token.type) {
					case OPEN_PARENTHESIS_TOKEN:
					case CLOSE_PARENTHESIS_TOKEN:
						break;
					case OPEN_BRACE_TOKEN:
						depth++;
						assert_spaces(i, 1);
						break;
					case CLOSE_BRACE_TOKEN:
						break;
					case PLUS_TOKEN:
						assert_spaces(i, 1);
						break;
					case MINUS_TOKEN:
						break;
					case MULTIPLICATION_TOKEN:
					case DIVISION_TOKEN:
					case REMAINDER_TOKEN:
					case COMMA_TOKEN:
						assert_spaces(i, 1);
						break;
					case COLON_TOKEN:
					case EQUALS_TOKEN:
					case NOT_EQUALS_TOKEN:
					case ASSIGNMENT_TOKEN:
					case GREATER_OR_EQUAL_TOKEN:
					case GREATER_TOKEN:
					case LESS_OR_EQUAL_TOKEN:
					case LESS_TOKEN:
					case NOT_TOKEN:
					case TRUE_TOKEN:
					case FALSE_TOKEN:
						break;
					case IF_TOKEN:
						assert_spaces(i, depth * SPACES_PER_INDENT);
						break;
					case ELSE_TOKEN:
						assert_spaces(i, 1);
						break;
					case LOOP_TOKEN:
					case BREAK_TOKEN:
					case RETURN_TOKEN:
					case CONTINUE_TOKEN:
						assert_spaces(i, depth * SPACES_PER_INDENT);
						break;
					case SPACES_TOKEN:
						abort();
					case NEWLINES_TOKEN:
						GRUG_ERROR("Unexpected trailing whitespace '%.*s' at token index %zu", (int)token.len, token.str, i);
					case STRING_TOKEN:
						break;
					case FIELD_NAME_TOKEN:
						assert_spaces(i, depth * SPACES_PER_INDENT);
						break;
					case WORD_TOKEN:
						break;
					case NUMBER_TOKEN:
						break;
					case COMMENT_TOKEN:
						// TODO: Ideally we'd assert there only ever being 1 space,
						// but the problem is that a standalone comment is allowed to have indentation
						// assert_spaces(i, 1);

						if (next_token.len < 2 || next_token.str[1] != ' ') {
							GRUG_ERROR("Expected the comment token '%.*s' to start with a space character at token index %zu", (int)next_token.len, next_token.str, i + 1);
						}

						if (next_token.len < 3 || isspace(next_token.str[2])) {
							GRUG_ERROR("Expected the comment token '%.*s' to have a text character directly after the space at token index %zu", (int)next_token.len, next_token.str, i + 1);
						}

						if (isspace(next_token.str[next_token.len - 1])) {
							GRUG_ERROR("Unexpected trailing whitespace in the comment token '%.*s' at token index %zu", (int)next_token.len, next_token.str, i + 1);
						}

						break;
				}
				break;
			}
			case NEWLINES_TOKEN:
			case STRING_TOKEN:
			case FIELD_NAME_TOKEN:
			case WORD_TOKEN:
			case NUMBER_TOKEN:
			case COMMENT_TOKEN:
				break;
		}

		// We're trimming all spaces in a single pass by copying every
		// non-space token to the start
		if (token.type != SPACES_TOKEN) {
			tokens[new_index] = token;
			new_index++;
		}

		i++;
	}

	if (depth > 0) {
		GRUG_ERROR("There were more '{' than '}'");
	}

	tokens_size = new_index;
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
        GRUG_ERROR("fopen");
	}

	if (fseek(f, 0, SEEK_END)) {
        GRUG_ERROR("fseek");
	}

	long count = ftell(f);
	if (count == -1) {
        GRUG_ERROR("ftell");
	}

	rewind(f);

	char *text = malloc(count + 1);
	if (!text) {
		GRUG_ERROR("malloc");
	}

	ssize_t bytes_read = fread(text, 1, count, f);
	if (bytes_read != count) {
        GRUG_ERROR("fread");
	}

	text[count] = '\0';

	return text;
}

void run() {
	char *grug_text = read_file("zombie.grug");
	printf("grug_text:\n%s\n", grug_text);

	reset();

	tokenize(grug_text);
	printf("After tokenize():\n");
	print_tokens();

	verify_and_trim_space_tokens();
	printf("After verify_and_trim_space_tokens():\n");
	print_tokens();

	parse();
	printf("\nfns:\n");
	print_fns();

	char *c_text = serialize_to_c();
	(void)c_text;
	// printf("c_text:\n%s\n", c_text);

	free(grug_text);
}

void error_handler(char *error_msg) {
	fprintf(stderr, "%s in %s:%d\n", error_msg, __FILE__, error_line);
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
	(void)helper_fns;
}
