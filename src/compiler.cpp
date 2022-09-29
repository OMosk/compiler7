#include "compiler.hpp"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <libgen.h>
#include <linux/limits.h>

thread_local Allocator thread_local_allocator;

void Scope::add(const char *name, AST *lc) {
  entries.append({.name = name, .ast = lc});
}

AST *Scope::find(const char *name, bool traverse) {
  // Due to the way how implement binding of values to names same identifier
  // can be present in scope multiple times. We are only interested in last one
  // at the given moment of typeching
  for (int i = entries.len - 1; i >= 0; --i) {
    if (strcmp(entries[i].name, name) == 0) {
      auto result = entries[i].ast;
      return result;
    }
  }

  if (traverse) {
    if (parent_scope) {
      auto result = parent_scope->find(name);
      if (result) {
        return result;
      }
    }
  }

  return NULL;
}

const char *to_string(Syntax_Node_Type value) {
  switch (value) {
    case SYNTAX_STATEMENT_BOUNDARY: return "SYNTAX_STATEMENT_BOUNDARY";
    case SYNTAX_IDENTIFIER: return "SYNTAX_IDENTIFIER";
    case SYNTAX_STRING_LITERAL: return "SYNTAX_STRING_LITERAL";
    case SYNTAX_NUMBER_LITERAL: return "SYNTAX_NUMBER_LITERAL";
    case SYNTAX_STRUCT_LITERAL_FIELD: return "SYNTAX_STRUCT_LITERAL_FIELD";
    case SYNTAX_STRUCT_LITERAL: return "SYNTAX_STRUCT_LITERAL";
    case SYNTAX_TUPLE_LITERAL_ITEM: return "SYNTAX_TUPLE_LITERAL_ITEM";
    case SYNTAX_TUPLE_LITERAL: return "SYNTAX_TUPLE_LITERAL";
    case SYNTAX_TUPLE_TYPE: return "SYNTAX_TUPLE_TYPE";
    case SYNTAX_UNION_TYPE: return "SYNTAX_UNION_TYPE";
    case SYNTAX_PROMOTION_TO_UNION: return "SYNTAX_PROMOTION_TO_UNION";
    case SYNTAX_BINARY_OP: return "SYNTAX_BINARY_OP";
    case SYNTAX_CALL: return "SYNTAX_CALL";
    case SYNTAX_SYSCALL: return "SYNTAX_SYSCALL";
    case SYNTAX_SUBSCRIPT: return "SYNTAX_SUBSCRIPT";
    case SYNTAX_CAST: return "SYNTAX_CAST";
    case SYNTAX_MEMBER_ACCESS: return "SYNTAX_MEMBER_ACCESS";
    case SYNTAX_BLOCK: return "SYNTAX_BLOCK";
    case SYNTAX_IF_STATEMENT: return "SYNTAX_IF_STATEMENT";
    case SYNTAX_CASE_STATEMENT: return "SYNTAX_CASE_STATEMENT";
    case SYNTAX_CASE_STATEMENT_BRANCH: return "SYNTAX_CASE_STATEMENT_BRANCH";
    case SYNTAX_FUNCTION_ARGUMENT_GROUP: return "SYNTAX_FUNCTION_ARGUMENT_GROUP";
    case SYNTAX_FUNCTION_ARGUMENT: return "SYNTAX_FUNCTION_ARGUMENT";
    case SYNTAX_FUNCTION: return "SYNTAX_FUNCTION";
    case SYNTAX_FUNCTION_TYPE: return "SYNTAX_FUNCTION_TYPE";
    case SYNTAX_STRUCT_FIELD_GROUP: return "SYNTAX_STRUCT_FIELD_GROUP";
    case SYNTAX_STRUCT_FIELD: return "SYNTAX_STRUCT_FIELD";
    case SYNTAX_STRUCT_DECLARATION: return "SYNTAX_STRUCT_DECLARATION";
    case SYNTAX_UNARY_OP: return "SYNTAX_UNARY_OP";
    case SYNTAX_LOAD_DIRECTIVE: return "SYNTAX_LOAD_DIRECTIVE";
    case SYNTAX_PATTERN_MATCHING: return "SYNTAX_PATTERN_MATCHING";
    case SYNTAX_TRUE: return "SYNTAX_TRUE";
    case SYNTAX_FALSE: return "SYNTAX_FALSE";
    case SYNTAX_FILE: return "SYNTAX_FILE";
    case SYNTAX_ATOM: return "SYNTAX_ATOM";
    case SYNTAX_LIST_LITERAL: return "SYNTAX_LIST_LITERAL";
  }
}

Compiler compiler;

__attribute__((__format__(__printf__, 1, 2)))
const char *alloc_sprintf(const char *fmt, ...) {
  va_list args;
  va_start(args, fmt);
  int want_len = vsnprintf(NULL, 0, fmt, args);
  va_end(args);

  va_start(args, fmt);
  char *buffer;
  buffer = alloc<char>(want_len + 1/*nul byte*/);
  int written = vsnprintf(buffer, want_len + 1, fmt, args);
  assert(written == want_len);
  va_end(args);

  return buffer;
}

DecodedCodeLoc decode_code_location(CodeLoc loc) {
  if (!loc.file_content || !loc.file_name) return {};

  int line = 1;
  int col = 1;
  for (uint32_t i = 0; i < loc.offset0; i++) {
    col += 1;
    if (loc.file_content[i] == '\n') {
      line += 1;
      col = 1;
    }
  }

  DecodedCodeLoc result = {};
  result.file_name = loc.file_name;
  result.line = line;
  result.col = col;
  return result;
}

void print_code_location(CodeLoc loc, FILE *out) {
  DecodedCodeLoc decoded = decode_code_location(loc);
  fprintf(out, "%s:%d:%d", decoded.file_name, decoded.line, decoded.col);
}

void print_code_location(CodeLoc *loc, FILE *out) {
  if (loc) {
    print_code_location(*loc, out);
  } else {
    CodeLoc empty = {};
    print_code_location(empty, out);
  }
}

__attribute__((__format__(__printf__, 2, 3)))
void report_error(CodeLoc loc, const char *fmt, ...) {
  printf("ERROR: ");

  va_list args;
  va_start(args, fmt);
  vprintf(fmt, args);
  va_end(args);

  printf("\n");

  if (loc.file_content && loc.file_name) {
    int line = 1;
    int col = 1;
    uint32_t line_start_offset = 0;
    for (uint32_t i = 0; i < loc.offset0; i++) {
      col += 1;
      if (loc.file_content[i] == '\n') {
        line += 1;
        col = 1;
        line_start_offset = i + 1;
      }
    }

    int line_len = 0;
    for (int i = line_start_offset; loc.file_content[i] != '\0' && loc.file_content[i] != '\n'; i++) {
      line_len++;
    }

    printf("  %s:%d:%d\n", loc.file_name, line, col);
    printf("%.*s\n", line_len, loc.file_content + line_start_offset);

    int error_position_in_line = loc.offset0 - line_start_offset;
    for (int i = 0; i < error_position_in_line; i++) printf(" ");
    printf("^\n");
  }
}

const char * read_file(const char *file_name, size_t *size_out) {
  FILE *f = fopen(file_name, "rb");
  if (!f) {
    // TODO: @ErrorReporting improvements. Show place where file was requested
    report_error({}, "Failed to open following file for reading: %s", file_name);
    exit(1);
  }

  fseek(f, 0, SEEK_END);
  size_t expected_file_size = ftell(f);
  fseek(f, 0, SEEK_SET);

  char *result = alloc<char>(expected_file_size + 1);

  size_t actually_read = fread(result, 1, expected_file_size, f);
  if (expected_file_size != actually_read) {
    report_error({}, "Error during file read: %s %s", file_name, strerror(errno));
    exit(1);
  }
  result[actually_read] = '\0';

  fclose(f);

  if (size_out) *size_out = actually_read;
  return result;
}


void schedule_file_load(AST *workspace, AST *module, const char *file_name, const char *relative_to_file = NULL) {
  const char *directory_name = NULL;
  if (relative_to_file) {
    auto copy = alloc_sprintf("%s", relative_to_file); // TODO: @MemoryLeak
    directory_name = dirname((char *)copy);
  } else {
    size_t len = PATH_MAX;
    auto buffer = alloc<char>(len); // TODO: @MemoryLeak
    directory_name = getcwd(buffer, len);
  }
  const char *file_name_to_read = alloc_sprintf("%s/%s", directory_name, file_name);

  for (int i = 0; i < module->files.len; ++i) {
    if (strcmp(file_name_to_read, module->files[i]) == 0) {
      //found we should be working on it already
      return;
    }
  }
  module->files.append(file_name_to_read);

  workspace->files_to_load.append({.file_name = file_name_to_read, .module = module});
}

void load_file(const char *file_name, AST *module) {

  size_t size = 0;
  const char *content = read_file(file_name, &size);

  Syntax *_file = parse_file(&compiler, content, file_name);

  auto file = syntax_assert_cast<Syntax_File>(_file);
  printf("Top level =%d\n", file->top_level_statements.len);
  //module->files.append(file_name);

  FOR (file->top_level_statements) {
    Syntax *node = *it;

    switch (node->type) {

    case SYNTAX_FUNCTION: {
      auto function = syntax_assert_cast<Syntax_Function>(node);
      if (!function->name) {
        report_error(function->loc, "Anonymous functions are not allowed in top level");
        exit(1);
      }

      AST *existing = module->scope.find(function->name->name);
      if (existing) {
        report_error(function->loc, "Duplicate declaration");
        report_error(existing->syntax->loc, "Previously found here");
        exit(1);
      }

      init_function_lc_if_necessary(function, &module->scope);
      auto function_lc = function->ast;
      //LC *function_lc = alloc<LC>();
      //function_lc->ast = function;
      //function_lc->value_kind = VALUE_KIND_FUNCTION;
      //function_lc->scope.parent_scope = &module->lc->scope;

      //function->lc = function_lc;

      module->scope.add(function->name->name, function_lc);

      auto workspace = module->workspace;
      workspace->ready_stack.push(function_lc);

      printf("%p -> %s\n", node, function->name->name);
      module->workspace->should_finish.append(node);
    } break;

    case SYNTAX_STRUCT_DECLARATION: {
      auto struct_ = syntax_assert_cast<Syntax_Struct_Declaration>(node);
    
      AST *existing = module->scope.find(struct_->name->name);
      if (existing) {
        report_error(struct_->loc, "Duplicate declaration");
        report_error(existing->syntax->loc, "Previously found here");
        exit(1);
      }

      AST *struct_lc = alloc<AST>();
      struct_lc->syntax = struct_;
      struct_lc->value_kind = VALUE_KIND_TYPE;
      struct_lc->scope.parent_scope = &module->scope;

      struct_->ast = struct_lc;

      module->scope.add(struct_->name->name, struct_lc);
    
      auto workspace = module->workspace;
      workspace->ready_stack.push(struct_lc);
    
      printf("%p -> %s\n", node, struct_->name->name);
      module->workspace->should_finish.append(node);
    } break;

    case SYNTAX_LOAD_DIRECTIVE: {
      auto *load_directive = syntax_assert_cast<Syntax_Load_Directive>(node);
      schedule_file_load(module->workspace, module, load_directive->path, file_name);
    } break;

    default: {
      printf("%s:%d:%s unhandled %s\n", __FILE__, __LINE__, __func__, to_string(node->type));
      assert(0 && "Not handled");
    }
    }
  }
}

AST *add_module(const char *entry_file_name, AST *workspace) {
  AST *module = NULL;
  module = alloc<AST>();
  module->value_kind = VALUE_KIND_MODULE;
  module->scope.parent_scope = &workspace->scope;

  module->path = entry_file_name;
  module->workspace = workspace;
  return module;
}

Type_Information *create_primitive_type(Scope *scope, const char *name, uint32_t size, uint32_t alignment) {
  Type_Information *ti = alloc<Type_Information>();
  ti->is_primitive_type = true;
  ti->size = size;
  ti->alignment = alignment;
  ti->name = name;

  AST *lc = alloc<AST>();
  lc->inferred_type = ti;
  lc->value_kind = VALUE_KIND_TYPE;
  lc->is_type_checking_done = true;

  scope->add(name, lc);

  return ti;
}



void init_builtin_types(AST *w) {
  w->builtin_types.void_ = create_primitive_type(&w->scope, "void", 0, 0);
  w->builtin_types.i64 = create_primitive_type(&w->scope, "i64", 8, 8);

  w->builtin_types.f64 = create_primitive_type(&w->scope, "f64", 8, 8);

  w->builtin_types.bool_ = create_primitive_type(&w->scope, "bool", 8, 8);

  w->builtin_types.rawptr = create_primitive_type(&w->scope, "rawptr", 8, 8);

  {
    auto s = alloc<Type_Information>();
    s->name = "string";
    s->is_struct_type = true;
    s->size = 16;
    s->alignment = 8;
    s->struct_fields.append({
      .type = w->builtin_types.rawptr,
      .name = "data",
    });
    s->struct_fields.append({
      .type = w->builtin_types.i64,
      .name = "len",
      .offset = 8,
    });

    AST *lc = alloc<AST>();
    lc->inferred_type = s;
    lc->value_kind = VALUE_KIND_TYPE;
    lc->is_type_checking_done = true;

    w->builtin_types.string = s;
    w->scope.add(s->name, lc);
  }
}

void start_compilation(Compilation_Params params) {
  auto file_name = params.entry_file_name;

  AST *workspace = alloc<AST>();
  init_builtin_types(workspace);

  workspace->support_library_functions.dynamic_allocation
    = workspace->ir_hub.create_support_library_function("c7_dynamic_allocation");
  workspace->support_library_functions.dynamic_allocation->create_argument();

  workspace->support_library_functions.string_concat
    = workspace->ir_hub.create_support_library_function("c7_string_concat");
  workspace->support_library_functions.string_concat->create_argument();
  workspace->support_library_functions.string_concat->create_argument();

  workspace->support_library_functions.string_compare
    = workspace->ir_hub.create_support_library_function("c7_string_compare");
  workspace->support_library_functions.string_compare->create_argument();
  workspace->support_library_functions.string_compare->create_argument();

  AST *entry_module = add_module(file_name, workspace);
  schedule_file_load(workspace, entry_module, file_name);

  while (workspace->files_to_load.len) {
    FileLoadQueuePair pair = workspace->files_to_load.last();
    workspace->files_to_load.len -= 1;
    load_file(pair.file_name, pair.module);
  }

  while (workspace->ready_stack.top) {
    AST *node = workspace->ready_stack.pop();
    assert(node);

    printf("%p being typechecked\n", node);
    Type_Check_Helper helper = {};
    helper.workspace = workspace;
    typecheck(&helper, node->syntax, &entry_module->scope, {});

    printf("%p done\n", node);
    for (int i = 0; i < workspace->should_finish.len; ++i) {
      if (workspace->should_finish[i] == node->syntax) {
        workspace->should_finish[i] = workspace->should_finish.last();
        workspace->should_finish.len -= 1;
      }
    }
  }

  printf("Functions to generate %d\n", workspace->functions_to_generate.len);
  FOR (workspace->functions_to_generate) {
    AST *fn = *it;
    assert(fn->value_kind == VALUE_KIND_FUNCTION);
    generate_function_ir(workspace, fn);
  }

  FOR (workspace->should_finish) {
    Syntax *node = *it;
    int i = 5;
    (void) node;
    (void) i;
  }
  assert(workspace->should_finish.len == 0 && "Some nodes were not typechecked");

  workspace->ir_hub.debug();

  //backend_emit_using_linux_x64_nasm_ld(&workspace->ir_hub, "out.nasm");
  switch (params.backend) {
    case BACKEND_NASM_LD: {
      backend_emit_using_linux_x64_nasm_ld(&workspace->ir_hub, "out");
    } break;

    case BACKEND_X64: {
      backend_emit_using_linux_x64_ld(&workspace->ir_hub, "out");
    } break;
    default: assert(0 && "unexpected backend value");
  }
  //backend_emit_using_linux_x64_nasm_ld(&workspace->ir_hub, "out");
}
