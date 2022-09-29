#include "compiler.hpp"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

enum Token_Type {
  TOKEN_NULL,
  TOKEN_UNEXPECTED_SEQUENCE_OF_CHARS,
  TOKEN_UNSUPPORTED_ESCAPING_SEQUENCE,
  TOKEN_UNTERMINATED_STRING_LITERAL,
  TOKEN_UNKNOWN_DIRECTIVE,
  TOKEN_EOF,
  TOKEN_LINE_COMMENT,
  TOKEN_STRING_LITERAL,
  TOKEN_NUMBER_LITERAL,
  TOKEN_DOT,
  TOKEN_COMMA,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_MULTIPLY,
  TOKEN_DIVIDE,
  TOKEN_EXCLAMATION_MARK,
  TOKEN_EXCLAMATION_MARK_EQUALS,
  TOKEN_AMPERSAND,
  TOKEN_AMPERSAND_AMPERSAND,
  TOKEN_PIPE,
  TOKEN_PIPE_PIPE,
  TOKEN_CARET,
  TOKEN_TILDE,
  TOKEN_COLON,
  TOKEN_SEMICOLON,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_LEFT_BRACKET,
  TOKEN_RIGHT_BRACKET,
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_ANGLE_BRACKET,
  TOKEN_RIGHT_ANGLE_BRACKET,
  TOKEN_LEFT_ANGLE_BRACKET_LEFT_ANGLE_BRACKET,
  TOKEN_RIGHT_ANGLE_BRACKET_RIGHT_ANGLE_BRACKET,
  TOKEN_LEFT_ANGLE_BRACKET_EQUALS,
  TOKEN_RIGHT_ANGLE_BRACKET_EQUALS,
  TOKEN_EQUALS,
  TOKEN_EQUALS_EQUALS,
  TOKEN_PERCENT,
  TOKEN_IDENTIFIER,

  TOKEN_LOAD_DIRECTIVE,
  TOKEN_SYSCALL_DIRECTIVE,
  TOKEN_BUILTIN_DIRECTIVE,
  TOKEN_C_CALL_DIRECTIVE,
  TOKEN_SUPPORT_LIBRARY_DIRECTIVE,

  TOKEN_ATOM,

  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_CASE,
  TOKEN_WHILE,
  TOKEN_DEFER,
  TOKEN_FUNC,
  TOKEN_ARROW,
  TOKEN_STRUCT,
  TOKEN_TRUE,
  TOKEN_FALSE,
};

const char *to_string(Token_Type value);

struct Token {
  uint32_t index;
  Token_Type type;
  CodeLoc loc;
  const char *value;

  uint32_t offset_after;
};

struct Lexer {
  const char *file_name;
  const char *content;
  size_t content_len;
  uint32_t token_index;
  Array<Token> tokens;

  Token peek() {
    Token result = tokens[token_index];
    return result;
  }

  Token eat() {
    Token result = peek();
    token_index += 1;
    return result;
  }

  void reset(Token token) {
    token_index = token.index;
  }

  void tokenize_file(const char *content, const char *file_name);

  Token match_single_line_comment(uint32_t offset);
  Token match_identifier_or_keyword_token(uint32_t offset);
  Token match_string_literal_token(uint32_t offset);
  Token match_number_literal_token(uint32_t offset);
  Token match_directive_token(uint32_t offset);
  Token match_atom(uint32_t offset);
  Token token_match(Token_Type type, uint32_t offset0, uint32_t offset1, const char *value);
  Token find_next_token(uint32_t offset);
};

void tokenize_file(Lexer *lexer, const char *content, const char *file_name) {
  lexer->tokenize_file(content, file_name);
}

bool is_alpha(int ch) {
  bool result =
      (('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || ch == '_');
  return result;
}
bool is_num(int ch) {
  bool result = ('0' <= ch && ch <= '9');
  return result;
}
bool is_alpha_num(int ch) {
  bool result = is_alpha(ch) || is_num(ch);
  return result;
}
bool is_whitespace(int ch) {
  bool result = (ch == ' ' || ch == '\t' || ch == '\n');
  return result;
}

bool has_prefix(const char *str, const char *prefix) {
  while (*str == *prefix && *str) {
    str++;
    prefix++;
  }
  if (*prefix != '\0')
    return false;
  return true;
}

Token Lexer::token_match(Token_Type type, uint32_t offset0, uint32_t offset1, const char *value) {
  assert(offset0 <= offset1);
  assert(offset0 <= content_len);
  assert(offset1 <= content_len);

  Token result = {};
  result.type = type;
  result.loc.file_content = content;
  result.loc.file_name = file_name;
  result.loc.offset0 = offset0;
  result.loc.offset1 = offset1;
  result.value = value;
  result.offset_after = offset1;
  return result;
}

Token Lexer::find_next_token(uint32_t offset) {
  while (offset < content_len && is_whitespace(content[offset]))
    offset++;

  if (offset == content_len) {
    return token_match(TOKEN_EOF, offset, offset, NULL);
  }

#define IF_PREFIX_RETURN(LITERAL, TYPE)                                        \
  do {                                                                         \
    if (has_prefix(content + offset, (LITERAL)))                               \
      return token_match((TYPE), offset, offset + (sizeof(LITERAL) - 1), NULL);\
  } while (0)

  if (has_prefix(content + offset, "//")) {
    return match_single_line_comment(offset);
  }

  IF_PREFIX_RETURN(".", TOKEN_DOT);
  IF_PREFIX_RETURN(",", TOKEN_COMMA);
  IF_PREFIX_RETURN("+", TOKEN_PLUS);
  IF_PREFIX_RETURN("->", TOKEN_ARROW);
  IF_PREFIX_RETURN("-", TOKEN_MINUS);
  IF_PREFIX_RETURN("*", TOKEN_MULTIPLY);
  IF_PREFIX_RETURN("/", TOKEN_DIVIDE);
  IF_PREFIX_RETURN("!=", TOKEN_EXCLAMATION_MARK_EQUALS);
  IF_PREFIX_RETURN("!", TOKEN_EXCLAMATION_MARK);
  IF_PREFIX_RETURN("&&", TOKEN_AMPERSAND_AMPERSAND);
  IF_PREFIX_RETURN("&", TOKEN_AMPERSAND);
  IF_PREFIX_RETURN("||", TOKEN_PIPE_PIPE);
  IF_PREFIX_RETURN("|", TOKEN_PIPE);
  IF_PREFIX_RETURN("~", TOKEN_TILDE);
  IF_PREFIX_RETURN("^", TOKEN_CARET);
  IF_PREFIX_RETURN("==", TOKEN_EQUALS_EQUALS);
  IF_PREFIX_RETURN("=", TOKEN_EQUALS);
  IF_PREFIX_RETURN("<<", TOKEN_LEFT_ANGLE_BRACKET_LEFT_ANGLE_BRACKET);
  IF_PREFIX_RETURN("<=", TOKEN_LEFT_ANGLE_BRACKET_EQUALS);
  IF_PREFIX_RETURN("<", TOKEN_LEFT_ANGLE_BRACKET);
  IF_PREFIX_RETURN(">>", TOKEN_RIGHT_ANGLE_BRACKET_RIGHT_ANGLE_BRACKET);
  IF_PREFIX_RETURN(">=", TOKEN_RIGHT_ANGLE_BRACKET_EQUALS);
  IF_PREFIX_RETURN(">", TOKEN_RIGHT_ANGLE_BRACKET);
  IF_PREFIX_RETURN("%", TOKEN_PERCENT);

  if (content[offset] == ':' && is_alpha(content[offset+1])) {
    return match_atom(offset);
  }

  IF_PREFIX_RETURN(":", TOKEN_COLON);
  IF_PREFIX_RETURN(";", TOKEN_SEMICOLON);
  IF_PREFIX_RETURN("{", TOKEN_LEFT_BRACE);
  IF_PREFIX_RETURN("}", TOKEN_RIGHT_BRACE);
  IF_PREFIX_RETURN("[", TOKEN_LEFT_BRACKET);
  IF_PREFIX_RETURN("]", TOKEN_RIGHT_BRACKET);
  IF_PREFIX_RETURN("(", TOKEN_LEFT_PAREN);
  IF_PREFIX_RETURN(")", TOKEN_RIGHT_PAREN);

  if (has_prefix(content + offset, "\"")) {
    return match_string_literal_token(offset);
  }
  if (has_prefix(content + offset, "#")) {
    return match_directive_token(offset);
  }
  if (is_num(content[offset])) {
    return match_number_literal_token(offset);
  }
  if (is_alpha(content[offset]) || content[offset] == '$') {
    return match_identifier_or_keyword_token(offset);
  }

#undef IF_PREFIX_RETURN

  return token_match(TOKEN_UNEXPECTED_SEQUENCE_OF_CHARS, offset, offset, NULL);
}

Token Lexer::match_identifier_or_keyword_token(uint32_t offset) {
  uint32_t offset1 = offset + 1;

  // Following check can be skipped, because we performed check on calling side
  // before calling this function

  // if (!isAlpha(content.data[offset1]) return {TYPE_TOKEN_NO_TOKEN, offset,
  // offset1}

  while (offset1 < content_len && is_alpha_num(content[offset1]))
    offset1++;

#define IF_EQUALS_RETURN(LITERAL, TYPE)                                        \
  do {                                                                         \
    if ((offset1 - offset) == (sizeof(LITERAL) - 1) &&                         \
        strncmp(content + offset, LITERAL, offset1 - offset) == 0)             \
      return token_match(TYPE, offset, offset1, NULL);                                             \
  } while (0)

  IF_EQUALS_RETURN("struct", TOKEN_STRUCT);
  IF_EQUALS_RETURN("func", TOKEN_FUNC);
  IF_EQUALS_RETURN("defer", TOKEN_DEFER);
  IF_EQUALS_RETURN("if", TOKEN_IF);
  IF_EQUALS_RETURN("else", TOKEN_ELSE);
  IF_EQUALS_RETURN("case", TOKEN_CASE);
  IF_EQUALS_RETURN("while", TOKEN_WHILE);
  IF_EQUALS_RETURN("true", TOKEN_TRUE);
  IF_EQUALS_RETURN("false", TOKEN_FALSE);

#undef IF_EQUALS_RETURN

  int identifier_len = offset1 - offset + 1;
  char *copy = alloc<char>(identifier_len);
  memcpy(copy, content + offset, identifier_len - 1);

  return token_match(TOKEN_IDENTIFIER, offset, offset1, copy);
}

// TypeOffset matchLineComment(Str content, uint32_t offset) {
// }

Token Lexer::match_string_literal_token(uint32_t offset) {
  for (uint32_t offset1 = offset + 1; offset1 < content_len; ++offset1) {
    if (content[offset1 - 1] != '\\' && content[offset1] == '"') {
      // TODO: maybe store length to avoid computing it later
      int value_len = offset1 - offset;
      const char *start = content + offset + 1/*to skip " at the beginning*/;

      char *copy = alloc<char>(value_len);
      memcpy(copy, start, value_len - 1/*to skip " at the end*/);

      // TODO: handle escaping
      char *write_it = copy;
      const char *read_it = copy;

      bool escaping = false;
      uint32_t offset_copy = offset + 1; // skiping '"'
      for (; *read_it; read_it += 1, offset_copy += 1) {
      //while (*read_it) {
        if (escaping) {
          switch (*read_it) {
          case 'n': {
            *write_it = '\n';
          } break;

          case 't': {
            *write_it = '\t';
          } break;

          default: {
            return token_match(TOKEN_UNSUPPORTED_ESCAPING_SEQUENCE, offset_copy, offset_copy + 1, NULL);
          } break;
          }

          write_it += 1;
          escaping = false;
        } else {
          if (*read_it == '\\') {
            escaping = true;
            continue;
          } else {
            *write_it = *read_it;
            write_it += 1;
          }
        }
      }
      *write_it = '\0';

      return token_match(TOKEN_STRING_LITERAL, offset, offset1 + 1, copy);
    }
  }
  return token_match(TOKEN_UNTERMINATED_STRING_LITERAL, offset, content_len, NULL);
}

Token Lexer::match_number_literal_token(uint32_t offset) {
  uint32_t offset1 = offset + 1;
  while (is_num(content[offset1]))
    offset1++;

  if (content[offset1] == '.' && is_num(content[offset1 + 1])) {
    offset1++;
    while (is_num(content[offset1]))
      offset1++;
  }

  int value_len = offset1 - offset;
  const char *start = content + offset;

  char *copy = alloc<char>(value_len + 1);
  memcpy(copy, start, value_len);

  return token_match(TOKEN_NUMBER_LITERAL, offset, offset1, copy);
}

Token Lexer::match_directive_token(uint32_t offset) {
  uint32_t offset1 = offset + 1;

  while (offset1 < content_len && is_alpha_num(content[offset1]))
    offset1++;

#define IF_EQUALS_RETURN(LITERAL, TYPE)                                        \
  do {                                                                         \
    if (strncmp(content + offset, LITERAL, offset1 - offset) == 0)             \
      return token_match(TYPE, offset, offset1, NULL);                                             \
  } while (0)

  IF_EQUALS_RETURN("#load", TOKEN_LOAD_DIRECTIVE);
  IF_EQUALS_RETURN("#syscall", TOKEN_SYSCALL_DIRECTIVE);
  IF_EQUALS_RETURN("#builtin", TOKEN_BUILTIN_DIRECTIVE);
  IF_EQUALS_RETURN("#c_call", TOKEN_C_CALL_DIRECTIVE);
  IF_EQUALS_RETURN("#support_library", TOKEN_SUPPORT_LIBRARY_DIRECTIVE);

#undef IF_EQUALS_RETURN

  return token_match(TOKEN_UNKNOWN_DIRECTIVE, offset, offset1, NULL);
}

Token Lexer::match_atom(uint32_t offset) {
  uint32_t offset1 = offset + 2;
  while (offset1 < content_len && is_alpha_num(content[offset1])) {
    offset1++;
  }

  char *value_copy = alloc<char>(offset1 - offset + 1);
  strncpy(value_copy, content + offset, offset1 - offset);
  value_copy[offset1 - offset] = '\0';

  return token_match(TOKEN_ATOM, offset, offset1, value_copy);
}

Token Lexer::match_single_line_comment(uint32_t offset) {
  uint32_t offset1 = offset + 2;
  while(offset1 < content_len && content[offset1] != '\n') {
    offset1 += 1;
  }
  // To include '\n'
  offset1 += 1;
  return token_match(TOKEN_LINE_COMMENT, offset, offset1, NULL);
}

void Lexer::tokenize_file(const char *content,
                      const char *file_name) {
  Lexer *lexer = this;
  lexer->token_index = 0;
  lexer->content = content;
  lexer->file_name = file_name;

  lexer->content_len = strlen(content);

  lexer->tokens.len = 0;

  Token match_result = {};
  do {
    match_result = find_next_token(match_result.offset_after);
    switch (match_result.type) {
    case TOKEN_UNEXPECTED_SEQUENCE_OF_CHARS:
    case TOKEN_UNTERMINATED_STRING_LITERAL:
    case TOKEN_UNSUPPORTED_ESCAPING_SEQUENCE:
    case TOKEN_UNKNOWN_DIRECTIVE: {
      // TODO: Tokenization @ErrorReporting improvement
      // report_error(context, file_index, "error", STRING("Tokenization
      // error"),
      //        match_result.offset0);
      report_error(match_result.loc, "Tokenization error at offset %d", match_result.loc.offset0);
      exit(1);
    }
    default: {
    } break;
    }

    if (match_result.type == TOKEN_LINE_COMMENT) {

    } else {
      match_result.index = lexer->tokens.len;
      lexer->tokens.append(match_result);
    }

  } while (match_result.type != TOKEN_EOF);
}

//------------------------------------------------------------------------------

struct Parsing_Error {
  bool error;
  CodeLoc loc;
  const char *message;
};

struct Parsing_Context {
  Lexer *lexer;
  Parsing_Error *error;
};

typedef Syntax *(Parser_Function)(Parsing_Context *context);

Syntax *parse(Parser_Function *function, Parsing_Context *context);
//
// namespace {
void fill_error(Parsing_Context *ctx, const char *message) {
  ctx->error->error = true;
  ctx->error->loc = ctx->lexer->tokens[ctx->lexer->token_index].loc;
  ctx->error->message = message;
}

bool parser_match(Parsing_Context *ctx, Token_Type token_type, const char *message,
           Token *out_token) {
  if (ctx->lexer->peek().type != token_type) {
    fill_error(ctx, message);
    return false;
  } else {
    Token token = ctx->lexer->eat();
    if (out_token) {
      *out_token = token;
    }
    return true;
  }
}
#define MATCH(TOKEN_NAME, TOKEN_TYPE, MESSAGE) \
    Token TOKEN_NAME = {}; \
    if (!parser_match(ctx, TOKEN_TYPE, MESSAGE, &TOKEN_NAME)) return NULL

#define MATCH_SUB(NAME, FUNC_NAME) \
    Syntax *NAME = parse(FUNC_NAME, ctx); \
    if (!NAME) return NULL

#define MATCH_SUB_INTO(FUNC_NAME, ARRAY)                                       \
  do {                                                                         \
    Syntax *TEMP = parse(FUNC_NAME, ctx);                                      \
    if (!TEMP) return NULL;                                                    \
    (ARRAY).append(TEMP);                                                      \
  } while (0)

#define MATCH_SUB_CAST_INTO(FUNC_NAME, TYPE, ARRAY)                            \
  do {                                                                         \
    Syntax *TEMP = parse(FUNC_NAME, ctx);                                      \
    if (!TEMP) return NULL;                                                    \
    (ARRAY).append(syntax_assert_cast<TYPE>(TEMP));                            \
  } while (0)

#define MATCH_ALT(NAME, FUNC_NAME) \
    if (!NAME) NAME = parse(FUNC_NAME, ctx);

Syntax *definition(Parsing_Context *ctx);
Syntax *unary_expression(Parsing_Context *ctx);
Syntax *expression(Parsing_Context *ctx);
Syntax *expression_statement(Parsing_Context *ctx);
Syntax *statement_boundary(Parsing_Context *ctx);
Syntax *block(Parsing_Context *ctx);
Syntax *call_continuation(Parsing_Context *ctx);
Syntax *statement(Parsing_Context *ctx);

bool next_is_statement_boundary(Parsing_Context *ctx) {
  auto t = ctx->lexer->peek();
  auto boundary = parse(statement_boundary, ctx);
  ctx->lexer->reset(t);

  bool result = boundary != NULL;

  return result;
}


Syntax *syscall(Parsing_Context *ctx) {
  MATCH(token, TOKEN_SYSCALL_DIRECTIVE, "Expected #syscall");
  MATCH_SUB(call, call_continuation);

  call->loc.offset0 = token.loc.offset0;

  auto *syscall = syntax_new<Syntax_Syscall>(token.loc, call->loc);
  syscall->args = syntax_assert_cast<Syntax_Call>(call)->args;

  return syscall;
}

Syntax *identifier(Parsing_Context *ctx) {
  MATCH(token, TOKEN_IDENTIFIER, "Expected identifier");

  Syntax_Identifier *ident = syntax_new<Syntax_Identifier>(token.loc);
  ident->name = token.value;

  return (Syntax *)ident;
}

Syntax *true_(Parsing_Context *ctx) {
  MATCH(token, TOKEN_TRUE, "Expected 'true'");

  auto *v = syntax_new<Syntax_True>(token.loc);

  return v;
}

Syntax *false_(Parsing_Context *ctx) {
  MATCH(token, TOKEN_FALSE, "Expected 'false'");

  auto *v = syntax_new<Syntax_False>(token.loc);

  return v;
}

Syntax *atom(Parsing_Context *ctx) {
  MATCH(token, TOKEN_ATOM, "Expected 'false'");

  auto *v = syntax_new<Syntax_Atom>(token.loc);
  v->name = token.value;

  return v;
}

Syntax *string_literal(Parsing_Context *ctx) {
  MATCH(token, TOKEN_STRING_LITERAL, "Expected string literal");

  Syntax_String_Literal *literal = syntax_new<Syntax_String_Literal>(token.loc);
  literal->value = token.value;

  return (Syntax *)literal;
}

Syntax *number_literal(Parsing_Context *ctx) {
  MATCH(token, TOKEN_NUMBER_LITERAL, "Expected number literal");

  Syntax_Number_Literal *literal = syntax_new<Syntax_Number_Literal>(token.loc);
  literal->value = token.value;

  return (Syntax *)literal;
}

Syntax *paren_expr(Parsing_Context *ctx) {
  MATCH(opening_paren, TOKEN_LEFT_PAREN, "Expected '('");
  MATCH_SUB(expr, expression);
  MATCH(closing_paren, TOKEN_RIGHT_PAREN, "Expected ')'");

  if (expr->type == SYNTAX_BINARY_OP) {
    Syntax_Binary_Op *binary_op = syntax_assert_cast<Syntax_Binary_Op>(expr);
    binary_op->flags |= AST_FLAGS_EXPR_IN_PAREN;
    binary_op->loc.offset0 = opening_paren.loc.offset0;
    binary_op->loc.offset1 = closing_paren.loc.offset1;
  }

  return expr;
}

Syntax *call_continuation(Parsing_Context *ctx) {
  Array<Syntax *> args = {};

  MATCH(opening_paren, TOKEN_LEFT_PAREN, "Expected '('");
  while (ctx->lexer->peek().type != TOKEN_RIGHT_PAREN) {
    MATCH_SUB_INTO(expression, args);

    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat();
    } else {
      break;
    }
  }
  MATCH(closing_paren, TOKEN_RIGHT_PAREN, "Expected ')'");

  Syntax_Call *call = syntax_new<Syntax_Call>(opening_paren.loc, closing_paren.loc);
  call->args = args;

  return (Syntax *)call;
}

Syntax *subscript_continuation(Parsing_Context *ctx) {
  MATCH(opening_bracket, TOKEN_LEFT_BRACKET, "Expected '['");
  MATCH_SUB(index, expression);
  MATCH(closing_bracket, TOKEN_RIGHT_BRACKET, "Expected ']'");

  Syntax_Subscript *subscript = syntax_new<Syntax_Subscript>(opening_bracket.loc, closing_bracket.loc);
  subscript->index = index;

  return (Syntax *)subscript;
}

Syntax *cast_continuation(Parsing_Context *ctx) {
  MATCH(dot, TOKEN_DOT, "Expected '.'");
  MATCH(opening_paren, TOKEN_LEFT_PAREN, "Expected '('");
  MATCH_SUB(type_expr, expression);
  MATCH(closing_paren, TOKEN_RIGHT_PAREN, "Expected ')'");

  Syntax_Cast *cast = syntax_new<Syntax_Cast>(dot.loc, closing_paren.loc);
  cast->to_type_expr = type_expr;

  return (Syntax *)cast;
}

Syntax *member_access_continuation(Parsing_Context *ctx) {
  MATCH(dot, TOKEN_DOT, "Expected '.'");
  MATCH_SUB(ident, identifier);

  Syntax_Member_Access *ma = syntax_new<Syntax_Member_Access>(dot.loc, ident->loc);
  ma->field = syntax_assert_cast<Syntax_Identifier>(ident);

  return (Syntax *)ma;
}

Syntax *if_statement(Parsing_Context *ctx) {
  MATCH(if_, TOKEN_IF, "Expected 'if'");
  MATCH_SUB(condition_expr, expression);
  MATCH_SUB(then_branch, statement);

  Syntax *else_branch = NULL;
  if (ctx->lexer->peek().type == TOKEN_ELSE) {
    ctx->lexer->eat();
    else_branch = parse(statement, ctx);
    if (!else_branch) return NULL;
  }
  

  Syntax_If_Statement *if_statement = syntax_new<Syntax_If_Statement>(
    if_.loc, else_branch ? else_branch->loc : then_branch->loc);

  if_statement->cond = condition_expr;
  if_statement->then_branch = then_branch;
  if_statement->else_branch = else_branch;

  return (Syntax *)if_statement;
}

Syntax *explicit_typing(Parsing_Context *ctx) {
  MATCH(colon_token, TOKEN_COLON, "Expected ':'");
  MATCH_SUB(type_expr, expression);
  return type_expr;
}

Syntax *case_statement(Parsing_Context *ctx) {
  MATCH(case_, TOKEN_CASE, "Expected 'case'");
  MATCH_SUB(value, expression);
  MATCH(opening_brace, TOKEN_LEFT_BRACE, "Expected '{");

  Array<Syntax_Case_Statement_Branch *> branches = {};

  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACE) {
    MATCH_SUB(value, expression);
    Syntax *type_expr = parse(explicit_typing, ctx); // optional

    MATCH(arrow, TOKEN_ARROW, "Expected '->'");
    MATCH_SUB(body, statement);

    auto branch = syntax_new<Syntax_Case_Statement_Branch>(value->loc, body->loc);
    branch->value = value;
    branch->type_expr = type_expr;
    branch->body = body;

    branches.append(branch);
  }

  MATCH(closing_brace, TOKEN_RIGHT_BRACE, "Expected '}");

  auto case_statement = syntax_new<Syntax_Case_Statement>(case_.loc, closing_brace.loc);
  case_statement->value = value;
  case_statement->branches = branches;

  return case_statement;
}

Syntax *pattern_matching_statement(Parsing_Context *ctx) {
  MATCH_SUB(left_expr, expression);
  auto left_explicit_type_expr = parse(explicit_typing, ctx); // optional

  MATCH(eq_token, TOKEN_EQUALS, "Expected '='");

  MATCH_SUB(right_expr, expression);

  MATCH_SUB(boundary, statement_boundary);

  Syntax_Pattern_Matching *pattern_matching = syntax_new<Syntax_Pattern_Matching>(left_expr->loc, right_expr->loc);
  pattern_matching->left = left_expr;
  pattern_matching->right = right_expr;
  pattern_matching->left_explicit_type_expr = left_explicit_type_expr;
  return pattern_matching;
}

Syntax *statement(Parsing_Context *ctx) {
  Syntax *s = NULL;
  MATCH_ALT(s, block);
  //MATCH_ALT(s, definition);
  //MATCH_ALT(s, if_statement);
  //MATCH_ALT(s, while_loop);
  //MATCH_ALT(s, defer_statement);
  MATCH_ALT(s, pattern_matching_statement);
  MATCH_ALT(s, expression_statement);
  MATCH_ALT(s, case_statement);

  return s;
}

Syntax *block(Parsing_Context *ctx) {
  MATCH(opening_brace, TOKEN_LEFT_BRACE, "Expected '{'");

  Array<Syntax *> statements = {};
  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACE) {
    MATCH_SUB_INTO(statement, statements);
  }
  MATCH(closing_brace, TOKEN_RIGHT_BRACE, "Expected '}'");

  Syntax_Block *block = syntax_new<Syntax_Block>(opening_brace.loc, closing_brace.loc);
  block->statements = statements;

  return (Syntax *)block;
}

Syntax *function_type(Parsing_Context *ctx) {
  Array<Syntax *> arg_type_exprs = {};

  MATCH(func_keyword, TOKEN_FUNC, "Expected 'func' keyword");

  MATCH(opening_paren, TOKEN_LEFT_PAREN, "Expected '('");

  while (ctx->lexer->peek().type != TOKEN_RIGHT_PAREN) {
    MATCH_SUB_INTO(expression, arg_type_exprs);
    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat();
    } else {
      break;
    }
  }

  MATCH(closing_paren, TOKEN_RIGHT_PAREN, "Expected ')'");
  MATCH(arrow, TOKEN_ARROW, "Expected '->'");

  MATCH_SUB(return_type_expr, expression);

  Syntax_Function_Type *f = syntax_new<Syntax_Function_Type>(func_keyword.loc, return_type_expr->loc);

  f->arg_type_exprs = arg_type_exprs;
  f->return_type_expr = return_type_expr;

  return f;
}

Syntax *function(Parsing_Context *ctx) {
  Array<Syntax_Function_Argument_Group *> args = {};
  Syntax *return_type_expr = NULL;

  MATCH(func_keyword, TOKEN_FUNC, "Expected 'func' keyword");

  Syntax_Identifier *name = NULL;
  {
    auto tmp = parse(identifier, ctx);
    if (tmp) name = syntax_assert_cast<Syntax_Identifier>(tmp);
  }

  MATCH(opening_paren, TOKEN_LEFT_PAREN, "Expected '('");

  while (ctx->lexer->peek().type != TOKEN_RIGHT_PAREN) {
    Array<Syntax_Identifier *> parameter_names = {};

    while (ctx->lexer->peek().type != TOKEN_COLON) {
      MATCH_SUB_CAST_INTO(identifier, Syntax_Identifier, parameter_names);

      if (ctx->lexer->peek().type == TOKEN_COMMA) {
        ctx->lexer->eat();
      } else {
        break;
      }
    }

    MATCH(colon, TOKEN_COLON, "Expected ':'");
    MATCH_SUB(type_expr, expression);

    Syntax_Function_Argument_Group *group = syntax_new<Syntax_Function_Argument_Group>(parameter_names[0]->loc, type_expr->loc);
    group->names = parameter_names;
    group->type_expr = type_expr;
    args.append(group);

    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat();
    } else {
      break;
    }
  }

  MATCH(closing_paren, TOKEN_RIGHT_PAREN, "Expected ')'");

  if (ctx->lexer->peek().type == TOKEN_ARROW) {
    ctx->lexer->eat();

    MATCH_SUB(_r, expression);
    return_type_expr = _r;
  }

  bool builtin = false;
  bool c_call = false;
  bool support_library = false;
  Syntax *body = NULL;
  Token builtin_direactive_token = {};

  CodeLoc end_pos = {};
  for (bool should_continue = true; should_continue;) {
    auto token_type = ctx->lexer->peek().type;
    switch (token_type) {
      case TOKEN_BUILTIN_DIRECTIVE: {
        builtin = true;
        ctx->lexer->eat();
        end_pos = builtin_direactive_token.loc;
      } break;

      case TOKEN_C_CALL_DIRECTIVE: {
        c_call = true;
        ctx->lexer->eat();
        end_pos = builtin_direactive_token.loc;
      } break;

      case TOKEN_SUPPORT_LIBRARY_DIRECTIVE: {
        support_library = true;
        ctx->lexer->eat();
        end_pos = builtin_direactive_token.loc;
      } break;

      default: should_continue = false;
    }
  }

  if (builtin && support_library) {
    report_error(func_keyword.loc, "Can't combine #builtin and #support_library directives");
    exit(1);
  }

  bool should_parse_body = !(builtin || support_library);

  if (should_parse_body) {
    body = parse(block, ctx);
    if (!body) return NULL;
    end_pos = body->loc;
  }

  Syntax_Function *f = syntax_new<Syntax_Function>(func_keyword.loc, end_pos);

  f->name = name;
  f->syntax_arg_groups = args;
  f->return_type_expr = return_type_expr;
  f->builtin = builtin;
  f->c_call = c_call;
  f->support_library = support_library;

  if (should_parse_body) {
    f->body = syntax_assert_cast<Syntax_Block>(body);
  }

  return (Syntax *)f;
}

Syntax *tuple_literal(Parsing_Context *ctx) {
  MATCH(opening_brace, TOKEN_LEFT_BRACE, "Expected '{'");
  
  Array<Syntax_Tuple_Literal_Item *> items = {};
  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACE) {
    MATCH_SUB(item_value, expression);
    auto optional_explicit_type = parse(explicit_typing, ctx);

    auto end_loc = item_value->loc;
    if (optional_explicit_type) end_loc = optional_explicit_type->loc;

    auto item = syntax_new<Syntax_Tuple_Literal_Item>(item_value->loc, end_loc);
    item->value = item_value;
    item->optional_type_expr = optional_explicit_type;

    items.append(item);

    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat();
    } else {
      break;
    }
  }

  MATCH(closing_brace, TOKEN_RIGHT_BRACE, "Expected '}'");

  auto tuple = syntax_new<Syntax_Tuple_Literal>(opening_brace.loc, closing_brace.loc);
  tuple->items = items;

  return tuple;
}

Syntax *list_literal(Parsing_Context *ctx) {
  printf("Have list literal\n");
  MATCH(opening_brace, TOKEN_LEFT_BRACKET, "Expected '['");
  
  Syntax *explicit_item_type = NULL;
  Array<Syntax *> items = {};
  Syntax *rest = NULL;
  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACKET) {
    MATCH_SUB(item_value, expression);

    if (!explicit_item_type && ctx->lexer->peek().type == TOKEN_COLON) {
      ctx->lexer->eat();
      explicit_item_type = item_value;
      continue;
    } else {
      items.append(item_value);
    }

    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat();
    } else {
      break;
    }
  }

  if (items.len) {
    if (items.last()->type == SYNTAX_BINARY_OP) {
      auto last_binary_op = syntax_assert_cast<Syntax_Binary_Op>(items.last());
      if (last_binary_op->op == BINARY_OP_BITWISE_OR && (last_binary_op->flags & AST_FLAGS_EXPR_IN_PAREN) == 0) {
        rest = last_binary_op->right;
        items[items.len - 1] = last_binary_op->left;
      }
    }
  }


  MATCH(closing_brace, TOKEN_RIGHT_BRACKET, "Expected ']'");

  auto list = syntax_new<Syntax_List_Literal>(opening_brace.loc, closing_brace.loc);
  list->explicit_item_type = explicit_item_type;
  list->items = items;
  list->rest = rest;

  return list;
}

Syntax *struct_literal(Parsing_Context *ctx) {
  MATCH(percent, TOKEN_PERCENT, "Expected '%'");
  Syntax *name_ = parse(identifier, ctx);
  Syntax_Identifier *name = NULL;
  if (name_) {
    name = syntax_assert_cast<Syntax_Identifier>(name_);
  }
  MATCH(opening_brace, TOKEN_LEFT_BRACE, "Expected '{' ");

  Array<Syntax_Struct_Literal_Field *> fields = {};

  auto savepoint = ctx->lexer->peek();
  Syntax *existing_value = parse(unary_expression, ctx);
  if (existing_value) {
    if (ctx->lexer->peek().type == TOKEN_PIPE) {
      ctx->lexer->eat();
    } else {
      existing_value = NULL;
      ctx->lexer->reset(savepoint);
    }
  }

  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACE) {
    MATCH_SUB(name, identifier);
    CodeLoc end_loc = name->loc;
    Syntax *value = NULL;

    Syntax *optional_type_expr = parse(explicit_typing, ctx);

    if (ctx->lexer->peek().type == TOKEN_EQUALS) {
      ctx->lexer->eat();

      value = parse(expression, ctx);
      if (!value) return NULL;

      end_loc = value->loc;
    }

    auto *field = syntax_new<Syntax_Struct_Literal_Field>(name->loc, end_loc);
    field->name = syntax_assert_cast<Syntax_Identifier>(name);
    field->optional_type_expr = optional_type_expr;
    field->value = value;
    fields.append(field);

    if (ctx->lexer->peek().type == TOKEN_COMMA) {
      ctx->lexer->eat(); //continue
    } else {
      break;
    }
  }
  MATCH(closing_brace, TOKEN_RIGHT_BRACE, "Expected '}' ");

  auto literal = syntax_new<Syntax_Struct_Literal>(percent.loc, closing_brace.loc);
  literal->name = name;
  literal->existing_value = existing_value;
  literal->fields = fields;

  return literal;
}

Syntax *unary_expression(Parsing_Context *ctx) {
  Syntax_Unary_Op *leftmost_unary_op = NULL;
  Syntax_Unary_Op *last_unary_op = NULL;

  for (bool should_continue = true; should_continue;) {
    Token next_token = ctx->lexer->peek();
    Unary_Op op = UNARY_OP_NOOP;
    switch (next_token.type) {
    case TOKEN_PLUS:             op = UNARY_OP_PLUS; break;
    case TOKEN_MINUS:            op = UNARY_OP_MINUS; break;
    case TOKEN_MULTIPLY:         op = UNARY_OP_DEREFERENCE; break;
    case TOKEN_AMPERSAND:        op = UNARY_OP_ADDRESSOF; break;
    case TOKEN_EXCLAMATION_MARK: op = UNARY_OP_LOGICAL_NEGATE; break;
    case TOKEN_TILDE:            op = UNARY_OP_BITWISE_NEGATE; break;
    default:
      should_continue = false;
    }

    if (should_continue) {
      ctx->lexer->eat();

      Syntax_Unary_Op *unary_op = syntax_new<Syntax_Unary_Op>(next_token.loc);
      unary_op->op = op;
      if (last_unary_op) {
        last_unary_op->operand = (Syntax *)unary_op;
      }
      last_unary_op = unary_op;
      if (!leftmost_unary_op) {
        leftmost_unary_op = unary_op;
      }
    }
  }

  Syntax *operand = NULL;
  MATCH_ALT(operand, identifier);
  MATCH_ALT(operand, string_literal);
  MATCH_ALT(operand, number_literal);
  MATCH_ALT(operand, struct_literal);
  MATCH_ALT(operand, tuple_literal);
  MATCH_ALT(operand, function);
  MATCH_ALT(operand, function_type);
  MATCH_ALT(operand, paren_expr);
  MATCH_ALT(operand, syscall);
  MATCH_ALT(operand, true_);
  MATCH_ALT(operand, false_);
  MATCH_ALT(operand, atom);
  MATCH_ALT(operand, list_literal);
  MATCH_ALT(operand, if_statement);

  if (!operand) {
    fill_error(ctx, "Expected unary expression operand");
    return NULL;
  }

  for (;;) {
    if (next_is_statement_boundary(ctx)) {
      break;
    }
    Syntax *continuation = NULL;
    MATCH_ALT(continuation, call_continuation);
    MATCH_ALT(continuation, subscript_continuation);
    MATCH_ALT(continuation, cast_continuation);
    MATCH_ALT(continuation, member_access_continuation);

    if (continuation) {
      continuation->loc.offset0 = operand->loc.offset0;

      switch (continuation->type) {
        case SYNTAX_CALL: syntax_assert_cast<Syntax_Call>(continuation)->callee = operand; break;
        case SYNTAX_SUBSCRIPT: syntax_assert_cast<Syntax_Subscript>(continuation)->array = operand; break;
        case SYNTAX_CAST: syntax_assert_cast<Syntax_Cast>(continuation)->value = operand; break;
        case SYNTAX_MEMBER_ACCESS: syntax_assert_cast<Syntax_Member_Access>(continuation)->value = operand; break;
        default: break;
      }

      operand = continuation;
    } else {
      break;
    }
  }

  if (last_unary_op) {
    last_unary_op->operand = operand;
  }
  if (leftmost_unary_op) {
    return (Syntax *)leftmost_unary_op;
  } else {
    return operand;
  }
}

int binary_op_priority(Binary_Op op) {
  switch (op) {
    case BINARY_OP_NOOP: assert(0 && "priority called on BINARY_OP_NOOP"); break;
    case BINARY_OP_LOGICAL_AND: return -7;
    case BINARY_OP_LOGICAL_OR: return -6;
    case BINARY_OP_BITWISE_OR: return -5;
    case BINARY_OP_BITWISE_AND: return -4;
    case BINARY_OP_BITWISE_XOR: return -3;
    case BINARY_OP_EQUAL: return -2;
    case BINARY_OP_NOT_EQUAL: return -2;
    case BINARY_OP_LESS: return -1;
    case BINARY_OP_LESS_OR_EQUAL: return -1;
    case BINARY_OP_GREATER: return -1;
    case BINARY_OP_GREATER_OR_EQUAL: return -1;
    case BINARY_OP_BITWISE_SHIFT_LEFT: return 0;
    case BINARY_OP_BITWISE_SHIFT_RIGHT: return 0;
    case BINARY_OP_PLUS: return 1;
    case BINARY_OP_MINUS: return 1;
    case BINARY_OP_MULTIPLY: return 2;
    case BINARY_OP_DIVISION: return 2;
    case BINARY_OP_REMAINDER: return 2;
    default: assert(0 && "unknown Binary_Op value");
  }
}

Syntax *expression(Parsing_Context *ctx) {
  MATCH_SUB(left, unary_expression);

  for (;;) {
    if (next_is_statement_boundary(ctx)) {
      break;
    }

    Binary_Op op = BINARY_OP_NOOP;

    Token_Type op_token_type = ctx->lexer->peek().type;
    CodeLoc op_loc = ctx->lexer->peek().loc;
    switch (op_token_type) {
    case TOKEN_PLUS: op = BINARY_OP_PLUS; break;
    case TOKEN_MINUS: op = BINARY_OP_MINUS; break;

    case TOKEN_MULTIPLY: op = BINARY_OP_MULTIPLY; break;
    case TOKEN_DIVIDE: op = BINARY_OP_DIVISION; break;
    case TOKEN_PERCENT: op = BINARY_OP_REMAINDER; break;

    case TOKEN_AMPERSAND: op = BINARY_OP_BITWISE_AND; break;
    case TOKEN_PIPE: op = BINARY_OP_BITWISE_OR; break;
    case TOKEN_CARET: op = BINARY_OP_BITWISE_XOR; break;

    case TOKEN_AMPERSAND_AMPERSAND: op = BINARY_OP_LOGICAL_AND; break;
    case TOKEN_PIPE_PIPE: op = BINARY_OP_LOGICAL_OR; break;

    case TOKEN_LEFT_ANGLE_BRACKET: op = BINARY_OP_LESS; break;
    case TOKEN_LEFT_ANGLE_BRACKET_EQUALS: op = BINARY_OP_LESS_OR_EQUAL; break;
    case TOKEN_RIGHT_ANGLE_BRACKET: op = BINARY_OP_GREATER; break;
    case TOKEN_RIGHT_ANGLE_BRACKET_EQUALS: op = BINARY_OP_GREATER_OR_EQUAL; break;
    case TOKEN_EQUALS_EQUALS: op = BINARY_OP_EQUAL; break;
    case TOKEN_EXCLAMATION_MARK_EQUALS: op = BINARY_OP_NOT_EQUAL; break;

    case TOKEN_LEFT_ANGLE_BRACKET_LEFT_ANGLE_BRACKET: op = BINARY_OP_BITWISE_SHIFT_LEFT; break;
    case TOKEN_RIGHT_ANGLE_BRACKET_RIGHT_ANGLE_BRACKET: op = BINARY_OP_BITWISE_SHIFT_RIGHT; break;

    default: break;
    }

    if (!op) {
      break;
    }

    /*Token operationToken =*/ctx->lexer->eat();

    MATCH_SUB(right, unary_expression);

    Syntax_Binary_Op *binary_op = syntax_new<Syntax_Binary_Op>(left->loc, right->loc);

    binary_op->left = left;
    binary_op->right = right;
    binary_op->op = op;
    binary_op->op_loc = op_loc;

    if (left->type == SYNTAX_BINARY_OP) {
      Syntax_Binary_Op *left_binary_op = syntax_assert_cast<Syntax_Binary_Op>(left);
      bool left_op_is_in_paren = left_binary_op->flags & AST_FLAGS_EXPR_IN_PAREN;


      int left_priority = binary_op_priority(left_binary_op->op);
      int priority = binary_op_priority(binary_op->op);

      if (left_priority < priority && !left_op_is_in_paren) {
        // TODO: @Offsets Make sure if offsets are correct
        // We probably should reassign them here
        binary_op->left = left_binary_op->right;
        left_binary_op->right = (Syntax *)binary_op;

        binary_op = left_binary_op;
      }
    }

    left = (Syntax *)binary_op;
  }

  return left;
}


Syntax *expression_statement(Parsing_Context *ctx) {
  MATCH_SUB(expr, expression);

  MATCH_SUB(boundary, statement_boundary);
  return expr;
}

Syntax *statement_boundary(Parsing_Context *ctx) {
  Syntax_Statement_Boundary *boundary = syntax_new<Syntax_Statement_Boundary>();

  Token token = ctx->lexer->peek();
  if (token.type == TOKEN_SEMICOLON || token.type == TOKEN_EOF || token.index == 0) {
    boundary->loc = token.loc;
    if (token.type != TOKEN_EOF) {
      ctx->lexer->eat();
    }
    return (Syntax *)boundary;
  }

  assert(token.index > 0);
  uint32_t prev_token_end = ctx->lexer->tokens[token.index - 1].loc.offset1;
  for (uint32_t i = prev_token_end; i < token.loc.offset0; ++i) {
    if (ctx->lexer->content[i] == '\n') {
      boundary->loc = token.loc;
      boundary->loc.offset0 = i;
      boundary->loc.offset1 = i + 1;
      return (Syntax *)boundary;
    }
  }

  fill_error(ctx, "Expected statement boundary");
  return NULL;
}

Syntax *struct_(Parsing_Context *ctx) {
  MATCH(struct_keyword, TOKEN_STRUCT, "Expected 'struct' keyword");
  MATCH_SUB(name, identifier);
  MATCH(opening_brace, TOKEN_LEFT_BRACE, "Expected '{' ");

  Array<Syntax_Struct_Field_Group *> field_groups = {};

  while (ctx->lexer->peek().type != TOKEN_RIGHT_BRACE) {
    Array<Syntax_Identifier *> field_names = {};

    while (ctx->lexer->peek().type != TOKEN_COLON) {
      MATCH_SUB_CAST_INTO(identifier, Syntax_Identifier, field_names);

      if (ctx->lexer->peek().type == TOKEN_COMMA) {
        ctx->lexer->eat();
      } else {
        break;
      }
    }

    MATCH(colon, TOKEN_COLON, "Expected ':'");
    MATCH_SUB(type_expr, expression);

    auto *group
        = syntax_new<Syntax_Struct_Field_Group>(field_names[0]->loc, type_expr->loc);
    group->names = field_names;
    group->type_expr = type_expr;
    field_groups.append(group);

    if (parse(statement_boundary, ctx)) {
      // just continue
    } else {
      break;
    }
  }
  MATCH(closing_brace, TOKEN_RIGHT_BRACE, "Expected '}' ");
  MATCH_SUB(boundary, statement_boundary);

  Syntax_Struct_Declaration *decl = syntax_new<Syntax_Struct_Declaration>(struct_keyword.loc, closing_brace.loc);
  decl->name = syntax_assert_cast<Syntax_Identifier>(name);
  decl->syntax_field_groups = field_groups;

  return decl;
}

Syntax *load_directive(Parsing_Context *ctx) {
  MATCH(load_directive_token, TOKEN_LOAD_DIRECTIVE, "Expected '#load' directive");
  MATCH(string_literal_token, TOKEN_STRING_LITERAL, "Expected string literal");

  auto *directive = syntax_new<Syntax_Load_Directive>(load_directive_token.loc, string_literal_token.loc);
  directive->path = string_literal_token.value;

  return directive;
}

Syntax *top_level_statement(Parsing_Context *ctx) {
  Syntax *top_level_statement = NULL;
  MATCH_ALT(top_level_statement, function);
  MATCH_ALT(top_level_statement, struct_);
  MATCH_ALT(top_level_statement, load_directive);
  MATCH_SUB(boundary, statement_boundary);

  return top_level_statement;
}

Syntax *file(Parsing_Context *ctx) {
  Array<Syntax *> top_level_statements = {};

  while (ctx->lexer->peek().type != TOKEN_EOF) {
    MATCH_SUB_INTO(top_level_statement, top_level_statements);
  }

  MATCH(eof, TOKEN_EOF, "Expected EOF");

  Syntax_File *file = syntax_new<Syntax_File>(eof.loc); // @Lazy: Eh, maybe check if array has elements and use 0th
  file->path = ctx->lexer->file_name;
  file->top_level_statements = top_level_statements;

  return (Syntax *)file;
}

Parsing_Error get_furthest_error(Parsing_Error a, Parsing_Error b) {
  if (a.error && b.error) {
    if (a.loc.offset0 > b.loc.offset0) {
      return a;
    } else if (a.loc.offset0 < b.loc.offset0) {
      return b;
    } else {
      return a;
    }
  } else if (a.error) {
    return a;
  } else if (b.error) {
    return b;
  } else {
    return (Parsing_Error){};
  }
}

Syntax *parse(Parser_Function *function, Parsing_Context *context) {
  Parsing_Error old_error = *context->error;
  uint32_t old_token_index = context->lexer->token_index;

  Syntax *result = function(context);
  // TODO: Revert thread_local_allocator to previous state to keep memory usage low

  if (!result) {
    context->lexer->token_index = old_token_index;
    *context->error = get_furthest_error(old_error, *context->error);
  } else {
    // Following code is needed to support optionals. In case if something was
    // tried and failed but it is not a deal-breaker. Without following code
    // error will be kept and this is not what I want.
    *context->error = old_error;
  }

  return result;
}

Syntax *parse_file(Compiler *compiler, const char *content, const char *file_name) {
  // To reuse memory of Token array
  if (!compiler->lexer) {
    compiler->lexer = alloc<Lexer>();
  }
  tokenize_file(compiler->lexer, content, file_name);

  Parsing_Error error_ = {};
  Parsing_Error *error = &error_;
  Parsing_Context context = {};
  context.lexer = compiler->lexer;
  context.error = error;

  Syntax *_file = parse(file, &context);
  if (!_file) {
    report_error(error->loc, "Parsing error at byte %u: %s %s", error->loc.offset0, file_name, error->message);
    exit(1);
  }
  return _file;
}
