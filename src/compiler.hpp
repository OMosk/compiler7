#pragma once

#include <stdalign.h>
#include <stddef.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <stdarg.h>
#include <stdio.h>

struct Lexer;

struct Syntax;
struct Syntax_Statement_Boundary;
struct Syntax_Identifier;
struct Syntax_String_Literal;
struct Syntax_Number_Literal;
struct Syntax_Struct_Literal_Field;
struct Syntax_Struct_Literal;
struct Syntax_Binary_Op;
struct Syntax_Call;
struct Syntax_Syscall;
struct Syntax_Subscript;
struct Syntax_Cast;
struct Syntax_Member_Access;
struct Syntax_Block;
struct Syntax_Function_Argument_Group;
struct Syntax_Function_Argument;
struct Syntax_Function;
struct Syntax_Struct_Field_Group;
struct Syntax_Struct_Field;
struct Syntax_Struct_Declaration;
struct Syntax_Unary_Op;
struct Syntax_Atom;
struct Syntax_List_Literal;

struct Syntax_File;

struct IR_Value;
struct IR_Function;
struct IR_Block;
struct IR_Hub;

inline int64_t align(int64_t value, int64_t alignment) {
  if (value % alignment) {
    value += alignment - value % alignment;
  }
  assert(value % alignment == 0);
  return value;
}

struct Allocator {
  char *start;
  int64_t used;
  int64_t available;
  int64_t allocations;

  void *alloc(size_t size, size_t alignment) {
    if (size == 0) {
      return NULL;
    }

    used = align(used, alignment);

    char * result = start + used;

    used += size;
    if (used >= available) {
      assert(0 && "Allocator exhausted");
    }
    allocations++;

    memset(result, 0, size);

    return result;
  }
};

extern thread_local Allocator thread_local_allocator;

template<typename T>
T *alloc(int n = 1) {
  return (T *) thread_local_allocator.alloc(n * sizeof(T), alignof(T));
}

__attribute__((__format__(__printf__, 1, 2)))
const char *alloc_sprintf(const char *fmt, ...);

template<typename T>
struct Array {
  T *data;
  int32_t len, cap;

  void append(T value) {
    if (len == cap) {
      cap = cap ? cap * 2 : 8;
      T *new_data = alloc<T>(cap);
      memcpy(new_data, data, len * sizeof(new_data[0]));
      data = new_data;
    }
    data[len++] = value;
  }

  T& operator[](int index) {
    assert(index < len);
    return data[index];
  }

  T& last() {
    assert(len > 0);
    return data[len - 1];
  }

  template<typename CMP>
  void sort(CMP cmp) {
    for (int i = 0; i < len; ++i) {
      for (int j = 0; j < len - 1; j++) {
        if (!cmp(data[j], data[j+1])) {
          auto tmp = data[j];
          data[j] = data[j+1];
          data[j+1] = tmp;
        }
      }
    }
  }
};

#define FOR(ARRAY) for (auto it = (ARRAY).data; it < (ARRAY).data + (ARRAY).len; it++)
#define C_ARRAY_LEN(ARR) (sizeof(ARR) / sizeof((ARR)[0]))

struct Builder {
  char *data;
  int32_t len, cap;

  void grow_if_necessary(int incoming_data_len) {
    if (len + incoming_data_len > cap) {
      auto new_cap = 2 * cap;
      if (!new_cap) new_cap = 8;

      while (new_cap < len + incoming_data_len) {
        new_cap *= 2;
      }

      auto new_data = alloc<char>(new_cap);
      memcpy(new_data, data, len);
      data = new_data;
      cap = new_cap;
    }
  }

  void append_c_str(const char *c_str) {
    int c_str_len = strlen(c_str);
    grow_if_necessary(c_str_len);

    memcpy(data + len, c_str, c_str_len);
    len += c_str_len;
  }

  void append_byte(char ch) {
    grow_if_necessary(1);
    data[len] = ch;
    len += 1;
  }
};

struct Buffer {
  char *data;
  int32_t len, cap;

};

inline void reserve(Buffer *b, int new_cap) {
  if (new_cap > b->cap) { //assert maybe?
    auto new_data = alloc<char>(new_cap);
    memcpy(new_data, b->data, b->len);
    b->data = new_data;
    b->cap = new_cap;
  }
}

inline void grow_if_necessary(Buffer *b, int new_len) {
  if (new_len > b->cap) {
    auto new_cap = 2 * b->cap;
    if (!new_cap) new_cap = 8;

    while (new_cap < new_len) {
      new_cap *= 2;
    }

    reserve(b, new_cap);
  }
}

inline int32_t add_bytes(Buffer *b, const uint8_t *incoming_data, size_t incoming_len, size_t at_offset = -1) {
  int32_t start_writing_at = b->len;
  if (at_offset != size_t(-1)) {
    start_writing_at = at_offset;
  }
  grow_if_necessary(b, start_writing_at + incoming_len);

  int32_t current_offset = b->len;
  for (size_t i = 0; i < incoming_len; i++) {
    b->data[start_writing_at + i] = incoming_data[i];
  }

  if (int32_t(start_writing_at + incoming_len) > b->len) {
    b->len = start_writing_at + incoming_len;
  }

  return current_offset;
}

inline int32_t add_1b(Buffer *b, uint8_t data, size_t at_offset = -1) {
  return add_bytes(b, &data, 1, at_offset);
}

inline int32_t add_2b(Buffer *b, uint16_t data, size_t at_offset = -1) {
  union {
    uint16_t number;
    uint8_t bytes[2];
  } converted;
  converted.number = data;
  return add_bytes(b, converted.bytes, 2, at_offset);
}

inline int32_t add_4b(Buffer *b, uint32_t data, size_t at_offset = -1) {
  union {
    uint32_t number;
    uint8_t bytes[4];
  } converted;
  converted.number = data;
  return add_bytes(b, converted.bytes, 4, at_offset);
}

inline int32_t add_8b(Buffer *b, uint64_t data, size_t at_offset = -1) {
  union {
    uint64_t number;
    uint8_t bytes[8];
  } converted;
  converted.number = data;
  return add_bytes(b, converted.bytes, 8, at_offset);
}

inline int32_t zero_pad(Buffer *b, size_t incoming_len, size_t at_offset = -1) {
  int32_t start_writing_at = b->len;
  if (at_offset != size_t(-1)) {
    start_writing_at = at_offset;
  }
  grow_if_necessary(b, start_writing_at + incoming_len);

  int32_t current_offset = b->len;
  for (size_t i = 0; i < incoming_len; i++) {
    b->data[start_writing_at + i] = 0;
  }

  if (int32_t(start_writing_at + incoming_len) > b->len) {
    b->len = start_writing_at + incoming_len;
  }

  return current_offset;
}

struct CodeLoc {
  const char *file_name;
  const char *file_content;
  uint32_t offset0, offset1;
};

enum IR_Value_Type {
  IR_VALUE_NOOP,
  IR_VALUE_CONSTANT_GLOBAL,
  IR_VALUE_CONSTANT_STRING_STRUCT,
  IR_VALUE_CLOSURE_BASE,
  IR_VALUE_IMMEDIATE,
  IR_VALUE_REGISTER,
  IR_VALUE_ARGUMENT,
  IR_VALUE_STATIC_ALLOCA,
  IR_VALUE_FUNCTION,
  IR_VALUE_COUNT,
};

const char *to_string(IR_Value_Type type);

struct IR_Value {
  IR_Value_Type type;
  const char *name;

  // Constant global
  const char *data;
  size_t len;

  // // Function
  IR_Function *function; // pointer to itself for convenience and guard

  // Immediate
  union {
    int64_t imm_i64;
    double imm_f64;
  };

  // Register
  int register_number;

  // Argument
  int argument_number;

  // Static alloca
  int alloca_offset;

  // Needed for c_call, only set for function arguments
  bool is_floating_point;
};

enum IR_Instruction_Type {
  IR_INSTRUCTION_NOOP,
  IR_INSTRUCTION_RET,

  IR_INSTRUCTION_SIGNED_INT_ADD,
  IR_INSTRUCTION_SIGNED_INT_SUB,
  IR_INSTRUCTION_SIGNED_INT_MUL,
  IR_INSTRUCTION_SIGNED_INT_DIV,
  IR_INSTRUCTION_SIGNED_INT_REMAINDER,

  IR_INSTRUCTION_SIGNED_INT_BITWISE_OR,
  IR_INSTRUCTION_SIGNED_INT_BITWISE_AND,
  IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR,

  IR_INSTRUCTION_SIGNED_INT_LOGICAL_OR,
  IR_INSTRUCTION_SIGNED_INT_LOGICAL_AND,
  IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT,

  IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL,
  IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL,
  IR_INSTRUCTION_SIGNED_INT_CMP_LESS,
  IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL,
  IR_INSTRUCTION_SIGNED_INT_CMP_GREATER,
  IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL,

  IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT,
  IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT,

  IR_INSTRUCTION_FLOAT_ADD,
  IR_INSTRUCTION_FLOAT_SUB,
  IR_INSTRUCTION_FLOAT_MUL,
  IR_INSTRUCTION_FLOAT_DIV,

  IR_INSTRUCTION_FLOAT_CMP_EQUAL,
  IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL,
  IR_INSTRUCTION_FLOAT_CMP_LESS,
  IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL,
  IR_INSTRUCTION_FLOAT_CMP_GREATER,
  IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL,

  IR_INSTRUCTION_CAST_FLOAT_TO_INTEGER_USING_TRUNCATION,
  IR_INSTRUCTION_CAST_INTEGER_TO_FLOAT,

  IR_INSTRUCTION_CALL,
  IR_INSTRUCTION_TAIL_CALL,
  IR_INSTRUCTION_SYSCALL,

  IR_INSTRUCTION_FILL_WITH_ZEROES,
  IR_INSTRUCTION_COPY_BYTES,

  IR_INSTRUCTION_64BIT_LOAD,
  IR_INSTRUCTION_64BIT_STORE,

  IR_INSTRUCTION_COUNT,
  IR_INSTRUCTION_COND_JUMP,
  IR_INSTRUCTION_JUMP,
  IR_INSTRUCTION_VALUE_MERGE,
};

const char *to_string(IR_Instruction_Type type);

struct IR_Instruction {
  IR_Instruction *next;

  IR_Instruction_Type type;
  IR_Value *res;
  IR_Value *op1;
  IR_Value *op2;
  IR_Value *op3;

  Array<IR_Value *> call_args;
  IR_Value *closure;

  IR_Block *then_block, *else_block;

  CodeLoc *loc;
};

struct IR_Instructions_List {
  IR_Instruction *start;
  IR_Instruction *end;
};

inline void concat_instructions_lists(IR_Instruction **inst, IR_Instructions_List b) {
  if (b.start) {
    if (*inst) {
      (*inst)->next = b.start;
    }
    if (b.end) {
      (*inst) = b.end;
    }
  }
}

inline IR_Instruction *traverse_till_the_end(IR_Instruction *inst) {
  assert(inst);
  while (inst->next) inst = inst->next;
  return inst;
}

struct IR_Scratchpad;

struct IR_Block {
  const char *name;

  IR_Instruction start_stub;
};

struct IR_Scratchpad {
  Array<IR_Block *> blocks;
  Array<IR_Value *> temporaries;

  IR_Block *create_block();
  IR_Value *create_temporary();

  void incorporate(IR_Scratchpad *other);
};

struct AST;

struct IR_Writer {
  AST *workspace;
  IR_Hub *hub;
  IR_Scratchpad *scratchpad;
  //IR_Instructions_List instructions_list;
  IR_Instruction *last_instruction;
  CodeLoc *loc;

  IR_Instruction *create_instruction(IR_Instruction_Type type);
  void create_ret(IR_Value *result);

  IR_Value *create_two_operand_instruction(IR_Instruction_Type type, IR_Value *left, IR_Value *right);
  IR_Value *create_one_operand_instruction(IR_Instruction_Type type, IR_Value *left);

  IR_Value *create_int_add(IR_Value *left, IR_Value *right);
  IR_Value *create_int_mul(IR_Value *left, IR_Value *right);

  IR_Value *create_signed_int_cmp_equal(IR_Value *left, IR_Value *right);
  IR_Value *create_signed_int_cmp_not_equal(IR_Value *left, IR_Value *right);

  IR_Value *create_cast_float_to_integer(IR_Value *left);
  IR_Value *create_cast_integer_to_float(IR_Value *left);

  IR_Value *create_call(IR_Value *callee, Array<IR_Value *> args, IR_Value *closure);
  IR_Value *create_tail_call(IR_Value *callee, Array<IR_Value *> args, IR_Value *closure);
  IR_Value *create_syscall(Array<IR_Value *> args);
  void create_fill_with_zeroes(IR_Value *address, IR_Value *size);
  void create_copy_bytes(IR_Value *to, IR_Value *from, IR_Value *size);

  IR_Value *create_64bit_load(IR_Value *from);
  void create_64bit_store(IR_Value *to, IR_Value *v);

  void create_cond_jump(IR_Value *cond, IR_Block *then_block, IR_Block *else_block);
  void create_jump(IR_Block *block);

  IR_Value *create_value_merge(IR_Value *cond, IR_Value *then_value, IR_Value *else_value);
};


struct IR_Function: IR_Value { // TODO: make it inherit from IR_Value
  //const char *name;
  const char *language_level_name;
  bool entry_point;
  bool builtin;
  bool c_call;
  bool support_library;

  bool returns_float; // needed for c_call

  IR_Block *entry_block;
  Array<IR_Value *> args; // needed for c_call to inspect whether argument is floating point

  IR_Scratchpad scratchpad;

  // Stack
  //   return_address
  //   prev rbp value
  //   ------- rbp value points here
  //   closure_base_pointer
  //   argument 0
  //   argument 1
  //   ....
  //   -------
  //   temporaries
  //   ....
  //   -------
  //   static_allocas
  //   ....
  //   -------
  int static_allocas_offset_from_bottom;
  int temporaries_offset_from_bottom;
  int preamble_allocation_size;

  //int static_alloca;

  IR_Value *create_argument();
};

struct IR_Hub {
  Array<IR_Function *> functions;
  Array<IR_Value *> const_globals;
  Array<IR_Value *> const_strings_structs;

  IR_Function *create_function(Syntax_Function *func);
  IR_Function *create_support_library_function(const char *name);
  IR_Value *create_const_data(const char *data, size_t len, const char *name = NULL);
  IR_Value *create_const_string_struct(const char *data, size_t len, const char *name = NULL);

  void debug();
};

IR_Value *ir_closure_base();

IR_Value *ir_create_immediate_i64(int64_t v);
IR_Value *ir_create_immediate_f64(double v);
IR_Value *ir_dynamic_memory_allocation(
  IR_Writer *writer,
  IR_Value *size);

struct Scope {
  struct Pair {
    const char *name;
    AST *ast;
  };
  Array<Pair> entries;
  Scope *parent_scope;

  void add(const char *name, AST *lc);
  AST *find(const char *name, bool traverse = true);
};

struct Type_Information;

struct Type_Check_Params {
  bool last_statement;
  Type_Information *wanted_type;
  AST *enclosing_function;

  // for polymorphic functions arguments
  Array<Syntax_Identifier *> *should_save_possible_polymorphic_identifier_into;
};

struct AST_Stack_Node {
  AST_Stack_Node *next;
  AST *ast;
};

struct AST_Stack {
  AST_Stack_Node *top;

  void push(AST *ast) {
    auto *node = alloc<AST_Stack_Node>();
    node->next = top;
    node->ast = ast;
    top = node;
  }

  AST *pop() {
    if (!top) return NULL;
    AST *result = top->ast;
    top = top->next; // TODO: @MemoryLeak, use freelist or something
    return result;
  }
};

enum {
  AST_FLAGS_EXPR_IN_PAREN = 1 << 0,
};

enum Unary_Op {
  UNARY_OP_NOOP,
  UNARY_OP_PLUS,
  UNARY_OP_MINUS,
  UNARY_OP_DEREFERENCE,
  UNARY_OP_ADDRESSOF,
  UNARY_OP_LOGICAL_NEGATE,
  UNARY_OP_BITWISE_NEGATE,
};

enum Binary_Op {
  BINARY_OP_NOOP,
  BINARY_OP_LOGICAL_AND,
  BINARY_OP_LOGICAL_OR,
  BINARY_OP_BITWISE_OR,
  BINARY_OP_BITWISE_AND,
  BINARY_OP_BITWISE_XOR,
  BINARY_OP_EQUAL,
  BINARY_OP_NOT_EQUAL,
  BINARY_OP_LESS,
  BINARY_OP_LESS_OR_EQUAL,
  BINARY_OP_GREATER,
  BINARY_OP_GREATER_OR_EQUAL,
  BINARY_OP_BITWISE_SHIFT_LEFT,
  BINARY_OP_BITWISE_SHIFT_RIGHT,
  BINARY_OP_PLUS,
  BINARY_OP_MINUS,
  BINARY_OP_MULTIPLY,
  BINARY_OP_DIVISION,
  BINARY_OP_REMAINDER,
};

enum Syntax_Node_Type {
  SYNTAX_STATEMENT_BOUNDARY,
  SYNTAX_IDENTIFIER,
  SYNTAX_STRING_LITERAL,
  SYNTAX_NUMBER_LITERAL,
  SYNTAX_STRUCT_LITERAL_FIELD,
  SYNTAX_STRUCT_LITERAL,
  SYNTAX_TUPLE_LITERAL_ITEM,
  SYNTAX_TUPLE_LITERAL,
  SYNTAX_BINARY_OP,
  SYNTAX_CALL,
  SYNTAX_SYSCALL,
  SYNTAX_SUBSCRIPT,
  SYNTAX_CAST,
  SYNTAX_MEMBER_ACCESS,
  SYNTAX_BLOCK,
  SYNTAX_IF_STATEMENT,
  SYNTAX_CASE_STATEMENT,
  SYNTAX_CASE_STATEMENT_BRANCH,
  SYNTAX_FUNCTION_ARGUMENT_GROUP,
  SYNTAX_FUNCTION_ARGUMENT,
  SYNTAX_FUNCTION_TYPE,
  SYNTAX_FUNCTION,
  SYNTAX_STRUCT_FIELD_GROUP,
  SYNTAX_STRUCT_FIELD,
  SYNTAX_STRUCT_DECLARATION,
  SYNTAX_TUPLE_TYPE,
  SYNTAX_UNION_TYPE,
  SYNTAX_PROMOTION_TO_UNION,
  SYNTAX_UNARY_OP,
  SYNTAX_ATOM,
  SYNTAX_LOAD_DIRECTIVE,
  SYNTAX_PATTERN_MATCHING,
  SYNTAX_TRUE,
  SYNTAX_FALSE,
  SYNTAX_LIST_LITERAL,
  SYNTAX_FILE,
};

const char *to_string(Syntax_Node_Type value);

enum Value_Kind {
  VALUE_KIND_NOOP,
  VALUE_KIND_TYPE,
  VALUE_KIND_VALUE,
  VALUE_KIND_ATOM,
  VALUE_KIND_FUNCTION,
  VALUE_KIND_POLYMORPHIC_FUNCTION,
  VALUE_KIND_POLYMORPHIC_VALUE,
  VALUE_KIND_MODULE,
};

struct Type_Information;

struct FunctionSpecializationPair {
  Array<Type_Information *> call_args;
  AST *specialization;
};

typedef void Generate_IR_Fn(IR_Writer *ir_writer, AST *lc);

struct Case_Statement_Branch {
  int type_index;
  AST *variable_lc;
  AST *branch_lc;
};

struct Struct_Literal_Entries {
  const char *name;
  AST *value;
};

struct Builtin_Types {
  Type_Information *void_;
  Type_Information *string;
  Type_Information *i64;
  Type_Information *f64;
  Type_Information *bool_;
  Type_Information *rawptr;
};

struct Support_Library_Functions {
  IR_Function *dynamic_allocation;
  IR_Function *string_concat;
  IR_Function *string_compare;
};

struct FileLoadQueuePair {
  const char *file_name;
  AST *module;
};


struct AST {
  Syntax *syntax;
  bool is_type_checking_done;
  Type_Information *inferred_type;
  Value_Kind value_kind;
  bool is_lvalue_;

  Scope scope;

  AST *belongs_to_function;
  Generate_IR_Fn *ir_generate_fn;
  IR_Value *ir_value_;

  AST *polymorphic_function; // needs to be separate probably because of copy_value()

  union {
    struct {
      //Function
      IR_Function *ir_function;
      AST *specialization_of_polymorphic_function;
      Type_Information *closure_capture_struct;
      Array<AST *> captures;
      AST *body_lc;
    };

    struct {
      Array<Type_Information *> polymorphic_function_arg_types;
      Array<FunctionSpecializationPair> function_specializations;
      Array<const char *> polymorphic_function_poly_params;
    };

    struct {
      //Block and pattern matching
      Array<AST *> block_statements;
    };

    struct {
      //Unary op
      AST *unary_op_operand_lc;
    };

    struct {
      //Binary op
      AST *binary_op_left_operand_lc, *binary_op_right_operand_lc;
    };

    struct {
      //Ident
      AST *identifier_refers_to;
    };

    struct {
      //Call
      bool call_is_tail_call;
      AST *call_callee;
      Array<AST *> call_args;
    };

    struct {
      //Member access
      AST *member_access_struct;
      const char *member_access_field_name;
    };

    struct {
      //Struct literal
      AST *struct_literal_existing_value;
      Array<Struct_Literal_Entries> struct_literal_entries;
    };

    struct {
      //If statement
      AST *if_statement_cond, *if_statement_then, *if_statement_else;
      bool if_statement_should_fill_union, if_statement_should_merge_values;
    };

    struct {
      //Cast
      AST *cast_value;
    };

    struct {
      //Tuple literal
      Array<AST *> tuple_literal_items;
    };

    struct {
      //Subscript
      AST *subscript_array_like;
      int subscript_index;
    };

    struct {
      //Case statement
      AST *case_statement_cond;
      Array<Case_Statement_Branch> case_statement_branches;
    };

    struct {
      //Promotion to union
      AST *promotion_to_union_from;
      int promotion_to_union_type_index;
    };

    struct {
      //Promotion to function pointer
      AST *promotion_to_function_pointer_function;
      Scope *promotion_to_function_pointer_parent_scope;
    };

    struct {
      //Capture
      const char *capture_name;
      AST *capture_enclosing_function;
    };

    struct {
      //Module
      const char *path;
      Array<const char *> files;
      //Scope scope;
      AST *workspace;
    };

    struct {
      //List literal construction
      Array<AST *> list_literal_items;
      AST * list_literal_rest;
    };

    struct {
      //list head/tail ops
      AST *list;
    };

    struct {
      //Workspace
      IR_Hub ir_hub;

      Array<FileLoadQueuePair> files_to_load;
      AST_Stack ready_stack;
      Array<Syntax *> should_finish;

      Builtin_Types builtin_types;
      Support_Library_Functions support_library_functions;

      Array<Type_Information *> tuple_types;
      Array<Type_Information *> union_types;
      Array<Type_Information *> atom_types;
      Array<Type_Information *> function_types;
      Array<Type_Information *> list_types;

      Array<AST *> functions_to_generate;
    };
  };
};

IR_Value *get_rvalue(IR_Writer *ir_writer, AST *from);

void generate_ir(IR_Writer *ir_writer, AST *lc);
void generate_function_ir(AST *workspace, AST *function_lc);

struct Syntax {
  Syntax_Node_Type type;
  CodeLoc loc;
  uint32_t flags;
};

bool is_integral_type(Type_Information *type);

struct Syntax_Statement_Boundary: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STATEMENT_BOUNDARY;
};

struct Syntax_Identifier: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_IDENTIFIER;
  const char *name;
};

struct Syntax_String_Literal: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRING_LITERAL;
  const char *value;
};

struct Syntax_Number_Literal: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_NUMBER_LITERAL;
  const char *value;
};

struct Syntax_Struct_Literal_Field: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRUCT_LITERAL_FIELD;
  Syntax_Identifier *name;
  Syntax *optional_type_expr;
  Syntax *value;
};

struct Syntax_Struct_Literal: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRUCT_LITERAL;
  Syntax_Identifier *name;
  Syntax *existing_value;
  Array<Syntax_Struct_Literal_Field *> fields;
};

struct Syntax_Tuple_Literal_Item: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_TUPLE_LITERAL_ITEM;
  Syntax *value;
  Syntax *optional_type_expr;
};

struct Syntax_Tuple_Literal: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_TUPLE_LITERAL;
  Array<Syntax_Tuple_Literal_Item *> items;
};

struct Syntax_Binary_Op: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_BINARY_OP;
  Syntax *left;
  Syntax *right;
  Binary_Op op;
  CodeLoc op_loc;
};

struct Syntax_Call: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_CALL;
  Syntax *callee;
  Array<Syntax *> args;

  //bool tail_call;
};

struct Syntax_Syscall: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_SYSCALL;
  Array<Syntax *> args;
};

struct Syntax_Subscript: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_SUBSCRIPT;
  Syntax *array;
  Syntax *index;
};

struct Syntax_Cast: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_CAST;
  Syntax *value;
  Syntax *to_type_expr;
};

struct Syntax_Member_Access: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_MEMBER_ACCESS;
  Syntax *value;
  Syntax_Identifier *field;
};

struct Syntax_Block: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_BLOCK;
  Array<Syntax *> statements;
};

struct Syntax_If_Statement: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_IF_STATEMENT;
  Syntax *cond;
  Syntax *then_branch;
  Syntax *else_branch;
};

struct Syntax_Case_Statement_Branch: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_CASE_STATEMENT_BRANCH;
  Syntax *value;
  Syntax *type_expr;
  Syntax *body;
};

struct Syntax_Case_Statement: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_CASE_STATEMENT;
  Syntax *value;
  Array<Syntax_Case_Statement_Branch *> branches;

  enum class Stage { INITIAL, BRANCH_TYPES_DONE, BRANCH_BODIES_DONE } stage;
};

struct Struct_Field {
  Type_Information *type;
  const char *name;
  int offset;
  CodeLoc loc;
};

struct Tuple_Item {
  Type_Information *type;
  int offset;
};

struct Type_Information {
  CodeLoc loc;
  const char *name;
  uint32_t size, alignment;

  bool is_primitive_type;
  bool is_function_type;
  Array<Type_Information *> function_type_arguments;
  Type_Information *function_type_return;

  bool is_struct_type;
  Array<Struct_Field> struct_fields;

  bool is_tuple_type;
  Array<Tuple_Item> tuple_items;

  bool is_union_type;
  Array<Type_Information *> union_type_variants;

  bool is_atom_type;
  int atom_index;

  bool is_poly_stub;

  bool is_list_type;
  Type_Information *list_item_type;
};

bool types_are_equal(Type_Information *left, Type_Information *right);

bool find_struct_field(Type_Information *struct_type_information, const char *name, Struct_Field *field);

bool find_type_in_union(Type_Information *union_type, Type_Information *type, int *out_index);
void copy_value_to_union(IR_Writer *ir_writer, IR_Value *address, int index, AST *value);

struct Syntax_Function_Argument_Group: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FUNCTION_ARGUMENT_GROUP;
  Array<Syntax_Identifier *> names;
  Syntax *type_expr;
};

struct Syntax_Function_Argument: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FUNCTION_ARGUMENT;
  Syntax_Identifier *name;
  Syntax_Function   *function;
  int            argument_position;
};

struct Syntax_Function: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FUNCTION;
  Syntax_Identifier *name;
  Array<Syntax_Function_Argument_Group *> syntax_arg_groups;
  Array<Syntax_Function_Argument *> args;
  Syntax *return_type_expr;
  Syntax_Block *body;
  bool builtin;
  bool c_call;
  bool support_library;

  AST *ast;
};

struct Syntax_Function_Type: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FUNCTION_TYPE;
  Array<Syntax *> arg_type_exprs;
  Syntax *return_type_expr;
};

struct Syntax_Struct_Field_Group: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRUCT_FIELD_GROUP;
  Array<Syntax_Identifier *> names;
  Syntax *type_expr;
};

struct Syntax_Struct_Field: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRUCT_FIELD;
  Syntax_Identifier *name;
  Syntax_Struct_Declaration     *stuct_;
  int            field_position;
  int            offset;
};

struct Syntax_Struct_Declaration: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_STRUCT_DECLARATION;
  Array<Syntax_Struct_Field_Group *> syntax_field_groups;

  Syntax_Identifier *name;
  Array<Syntax_Struct_Field *> fields;

  AST *ast;
};

struct Syntax_Unary_Op: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_UNARY_OP;
  Syntax *operand;
  Unary_Op op;
};

struct Syntax_Atom: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_ATOM;
  const char *name;
};

struct Syntax_Load_Directive: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_LOAD_DIRECTIVE;
  const char *path;
};

struct Syntax_Pattern_Matching: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_PATTERN_MATCHING;
  Syntax *left, *right;
  Syntax *left_explicit_type_expr;

  Array<Syntax *> additional_code_to_generate_during_ir_traverse;
};

struct Syntax_True: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_TRUE;
};

struct Syntax_False: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FALSE;
};

struct Syntax_List_Literal: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_LIST_LITERAL;

  Syntax *explicit_item_type;
  Array<Syntax *> items;
  Syntax *rest;
};

struct Syntax_File: Syntax {
  static const Syntax_Node_Type TYPE = SYNTAX_FILE;
  const char *path;
  Array<Syntax *> top_level_statements;
};

template<typename T>
T *syntax_new(CodeLoc loc0 = {}, CodeLoc loc1 = {}) {
  T *result = alloc<T>();
  result->type = T::TYPE;
  result->loc = loc0;
  if (loc1.offset1) {
    result->loc.offset1 = loc1.offset1;
  } else {
    result->loc.offset1 = loc0.offset1;
  }
  return result;
}

template<typename T>
T *syntax_assert_cast(Syntax *from) {
  assert(from);
  if (from->type == T::TYPE) {
    return (T*) from;
  } else {
    assert(0 && "AST assert cast type mismatch");
  }
}

struct Compiler {
  Lexer *lexer;
};

Syntax *parse_file(Compiler *compiler, const char *content, const char *file_name);

__attribute__((__format__(__printf__, 2, 3)))
void report_error(CodeLoc loc, const char *fmt, ...);

IR_Value *get_lvalue(IR_Writer *w, AST *from);
IR_Value *get_rvalue(IR_Writer *ir_writer, AST *from);

void generate_binary_op_ir_for_floats(IR_Writer *w, AST *lc);
void generate_binary_op_ir_for_ints(IR_Writer *w, AST *lc);
void generate_capture_ir(IR_Writer *w, AST *lc);
void generate_if_statement_ir(IR_Writer *w, AST *lc);
void generate_short_circuited_logical_operation(IR_Writer *w, AST *lc, int64_t early_exit_cond_value);
void generate_string_comparison_ir(IR_Writer *w, AST *lc, IR_Instruction_Type cmp_op);
void generate_struct_literal_ir(IR_Writer *w, AST *lc);
void generate_struct_member_access_ir(IR_Writer *w, AST *lc);
void generate_tuple_subscript_ir(IR_Writer *w, AST *lc);

struct DecodedCodeLoc {
  const char *file_name;
  int line;
  int col;
};

DecodedCodeLoc decode_code_location(CodeLoc loc);
void print_code_location(CodeLoc loc, FILE *out);
void print_code_location(CodeLoc *loc, FILE *out);

enum Backend {
  BACKEND_NASM_LD,
  BACKEND_X64,
};

struct Compilation_Params {
  const char *entry_file_name;
  Backend backend;
};

void start_compilation(Compilation_Params params);

void backend_emit_using_linux_x64_nasm_ld(IR_Hub *hub, const char *to);
void backend_emit_using_linux_x64_ld(IR_Hub *hub, const char *to);

extern unsigned char support_library_o_data [];
extern unsigned long support_library_o_len;

struct Type_Check_Helper {
  AST *workspace;
};
AST *typecheck(Type_Check_Helper *helper, Syntax *node, Scope *parent_scope, Type_Check_Params params);
void init_function_lc_if_necessary(Syntax_Function *function, Scope *parent_scope);

extern Compiler compiler;

int calculate_offset(IR_Function *f, IR_Value *v);
