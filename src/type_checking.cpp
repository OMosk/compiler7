#include "compiler.hpp"

#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdarg.h>
#include <stdlib.h>
#include <unistd.h>
#include <libgen.h>
#include <linux/limits.h>

IR_Value *get_lvalue(IR_Writer *w, AST *from) {
  assert(from->is_lvalue_);
  return from->ir_value_;
}

IR_Value *get_rvalue(IR_Writer *ir_writer, AST *from) {
  if (from->is_lvalue_) {
    assert(is_integral_type(from->inferred_type));
    IR_Value *result = ir_writer->create_64bit_load(from->ir_value_);
    return result;
  } else {
    IR_Value *result = from->ir_value_;
    return result;
  }
}

bool find_struct_field(Type_Information *struct_type_information, const char *name, Struct_Field *out_field) {
  FOR (struct_type_information->struct_fields) {
    auto &field = *it;
    if (strcmp(field.name, name) == 0) {
      *out_field = field;
      return true;
    }
  }

  return false;
}

void copy_value(AST *to, AST *from) {
  assert(from->is_type_checking_done);
  assert(from->inferred_type);
  to->inferred_type = from->inferred_type;
  to->value_kind = from->value_kind;
  //to->ir_value_ = from->ir_value_;
  to->is_lvalue_ = from->is_lvalue_;
  to->polymorphic_function = from->polymorphic_function;
}

void set_value(AST *to, IR_Value *v, bool is_lvalue) {
  //to->ir_value_ = v;
  to->is_lvalue_ = is_lvalue;
  to->value_kind = VALUE_KIND_VALUE;
}

bool is_integral_type(Type_Information *type) {
  bool result = type->is_primitive_type;
  return result;
}

const char *type_to_string(Type_Information *t) {
  return t->name;
}

void report_type_mismatch_message(Type_Information *wanted, Type_Information *given) {
  report_error({}, "Wanted '%s' and was given '%s'", type_to_string(wanted), type_to_string(given));
}

bool types_are_equal(Type_Information *left, Type_Information *right) {
  assert(left);
  assert(right);

  return left == right;
}

Type_Information *resolve_function_type(AST *workspace, Array<Type_Information *> arg_types, Type_Information *return_type) {
  Type_Information *found = NULL;

  FOR (workspace->function_types) {
    auto existing = *it;
    if (existing->function_type_return != return_type) continue;
    if (existing->function_type_arguments.len != arg_types.len) continue;

    bool ok = true;
    for (int i = 0; i < arg_types.len; ++i) {
      if (arg_types[i] != existing->function_type_arguments[i]) {
        ok = false;
        break;
      }
    }
    if (!ok) continue;

    found = existing;
  }

  if (!found) {
    auto t = alloc<Type_Information>();
    t->size = 16;
    t->alignment = 8;
    t->is_function_type = true;
    t->function_type_arguments = arg_types;
    t->function_type_return = return_type;

    Builder name_builder = {};
    name_builder.append_c_str("func (");
    {
      bool first = true;
      FOR (arg_types) {
        Type_Information *arg_type = *it;
        if (!first) name_builder.append_c_str(", ");
        name_builder.append_c_str(type_to_string(arg_type));
        first = false;
      }
    }
    name_builder.append_c_str(") -> ");
    name_builder.append_c_str(type_to_string(return_type));
    name_builder.append_byte(0);

    t->name = name_builder.data;

    found = t;
    workspace->function_types.append(found);
  }

  return found;
}

bool is_a_value(AST *lc) {
  assert(lc->value_kind != VALUE_KIND_NOOP);
  bool result = lc->value_kind == VALUE_KIND_VALUE
    || lc->value_kind == VALUE_KIND_ATOM
    || lc->value_kind == VALUE_KIND_FUNCTION
    //|| lc->value_kind == VALUE_KIND_POLYMORPHIC_VALUE // it confuses list literal in with poly type inside
    ;
  return result;
}

void require_to_be_a_value(AST *ast) {
  if (!is_a_value(ast)) {
    report_error(ast->syntax->loc, "Does not refer to a value");
    exit(1);
  }
}

bool is_a_type(AST *lc) {
  assert(lc->value_kind != VALUE_KIND_NOOP);
  bool result = lc->value_kind == VALUE_KIND_TYPE
    || lc->value_kind == VALUE_KIND_ATOM
    || lc->value_kind == VALUE_KIND_POLYMORPHIC_VALUE
    ;
  return result;
}

void require_to_be_a_type(AST *lc) {
  if (!is_a_type(lc)) {
    report_error(lc->syntax->loc, "Does not refer to a type");
    exit(1);
  }
}

bool find_type_in_union(Type_Information *union_type, Type_Information *type, int *out_index) {
  int found = -1;
  for (int i = 0; i < union_type->union_type_variants.len; ++i) {
    if (union_type->union_type_variants[i] == type) {
      found = i;
      break;
    }
  }

  if (found == -1) {
    return false;
  }

  *out_index = found;

  return true;
}

void copy_ir_value(IR_Writer *w, IR_Value *data_address, AST *value) {
  if (is_integral_type(value->inferred_type)) {
    auto ir_value = get_rvalue(w, value);
    w->create_64bit_store(data_address, ir_value);
  } else {
    w->create_copy_bytes(data_address, get_lvalue(w, value), ir_create_immediate_i64(value->inferred_type->size));
  }
}

void copy_value_to_union(IR_Writer *ir_writer, IR_Value *address, int index, AST *value) {

  int tag_value = index + 1;
  ir_writer->create_64bit_store(address, ir_create_immediate_i64(tag_value));
  IR_Value *data_address = ir_writer->create_int_add(address, ir_create_immediate_i64(ir_writer->workspace->builtin_types.i64->size));
  copy_ir_value(ir_writer, data_address, value);
}

bool emit_promotion_to_union(AST *value, Type_Information *union_type, AST **out_result) {

  require_to_be_a_value(value);

  int found = -1;
  if (!find_type_in_union(union_type, value->inferred_type, &found)) {
    return false;
  }

  AST *result = alloc<AST>();
  result->syntax = value->syntax;
  result->value_kind = VALUE_KIND_VALUE;
  result->is_lvalue_ = true;
  result->inferred_type = union_type;
  result->promotion_to_union_from = value;
  result->promotion_to_union_type_index = found;
  result->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
    auto union_type = lc->inferred_type;
    auto value = lc->promotion_to_union_from;
    auto found = lc->promotion_to_union_type_index;
    generate_ir(w, value);
    IR_Value *address = ir_dynamic_memory_allocation(w, ir_create_immediate_i64(union_type->size));
    copy_value_to_union(w, address, found, value);
    lc->ir_value_ = address;
  };

  *out_result = result;

  return true;
}


void traverse_pattern_matching(Type_Check_Helper *helper, Scope *parent_scope, Syntax *left, AST *right, Array<AST *> *arr) {
  require_to_be_a_value(right);

  switch (left->type) {
  case SYNTAX_IDENTIFIER: {
    auto ident = syntax_assert_cast<Syntax_Identifier>(left);
    auto rhs_copy = alloc<AST>();
    *rhs_copy = *right;
    parent_scope->add(ident->name, rhs_copy);
    rhs_copy->identifier_refers_to = right;
    rhs_copy->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
      assert(lc->identifier_refers_to->ir_value_);
      lc->ir_value_ = lc->identifier_refers_to->ir_value_;
      //lc->is_lvalue_ = lc->identifier_refers_to->is_lvalue_;
    };
    arr->append(rhs_copy);
  } break;

  case SYNTAX_STRUCT_LITERAL: {
    auto struct_literal = syntax_assert_cast<Syntax_Struct_Literal>(left);
    if (!right->inferred_type->is_struct_type) {
      report_error(right->syntax->loc, "Need to be a struct type");
      exit(1);
    }
    auto struct_type = right->inferred_type;

    if (struct_literal->name) {
      Syntax *struct_literal_type_expr = struct_literal->name;

      auto struct_name_lc = typecheck(helper, struct_literal_type_expr, parent_scope, {});
      require_to_be_a_type(struct_name_lc);

      if (!types_are_equal(struct_name_lc->inferred_type, struct_type)) {
        report_error(struct_literal_type_expr->loc, "Pattern matching type mismatch");
        report_type_mismatch_message(struct_name_lc->inferred_type, struct_type);
        exit(1);
      }
    }

    FOR (struct_literal->fields) {
      Syntax_Struct_Literal_Field *literal_field = *it;
      const char *name = literal_field->name->name;

      Struct_Field field = {};
      if (!find_struct_field(struct_type, name, &field)) {
        report_error(literal_field->name->loc, "Field was not found at struct");
        report_error(struct_type->loc, "Struct was defined here");
        exit(1);
      }

      if (literal_field->optional_type_expr) {
        auto optional_field_type_lc
          = typecheck(helper, literal_field->optional_type_expr, parent_scope, {});
        require_to_be_a_type(optional_field_type_lc);

        if (!types_are_equal(field.type, optional_field_type_lc->inferred_type)) {
          report_error(literal_field->optional_type_expr->loc, "Explicit type does not match type of the struct field");
          report_type_mismatch_message(field.type, optional_field_type_lc->inferred_type);
          exit(1);
        }
      }

      AST *field_value_lc = alloc<AST>();
      field_value_lc->syntax = literal_field;
      field_value_lc->value_kind = VALUE_KIND_VALUE;
      field_value_lc->inferred_type = field.type;
      field_value_lc->is_type_checking_done = true;

      field_value_lc->member_access_struct = right;
      field_value_lc->member_access_field_name = name;

      field_value_lc->ir_generate_fn = generate_struct_member_access_ir;

      set_value(field_value_lc, NULL/*computed_address*/, true);

      arr->append(field_value_lc);
      if (literal_field->value) {
        traverse_pattern_matching(helper, parent_scope, literal_field->value, field_value_lc, arr);
      } else {
        traverse_pattern_matching(helper, parent_scope, literal_field->name, field_value_lc, arr);
      }
    }
  } break;

  case SYNTAX_TUPLE_LITERAL: {
    auto tuple_literal = syntax_assert_cast<Syntax_Tuple_Literal>(left);

    if (!right->inferred_type->is_tuple_type) {
      report_error(right->syntax->loc, "Can't be destructured into tuple, type = '%s'", type_to_string(right->inferred_type));
      exit(1);
    }

    auto tuple_type = right->inferred_type;
    //if (tuple_type->tuple_items.len)

    for (int i = 0; i < tuple_literal->items.len; i++) {
      Syntax_Tuple_Literal_Item *literal_item = tuple_literal->items[i];

      Tuple_Item tuple_type_item = tuple_type->tuple_items[i];

      if (literal_item->optional_type_expr) {
        auto optional_type_expr_lc = typecheck(helper, literal_item->optional_type_expr, parent_scope, {});
        require_to_be_a_type(optional_type_expr_lc);

        if (!types_are_equal(optional_type_expr_lc->inferred_type, tuple_type_item.type)) {
          report_error(literal_item->optional_type_expr->loc, "Explicit type does not match type on the right side");
          report_type_mismatch_message(optional_type_expr_lc->inferred_type, tuple_type_item.type);
          exit(1);
        }
      }


      AST *item_lc = alloc<AST>();
      item_lc->syntax = literal_item;
      item_lc->inferred_type = tuple_type_item.type;
      item_lc->is_type_checking_done = true;

      item_lc->subscript_array_like = right;
      item_lc->subscript_index = i;
      item_lc->ir_generate_fn = generate_tuple_subscript_ir;

      set_value(item_lc, NULL, true);

      arr->append(item_lc);
      traverse_pattern_matching(helper, parent_scope, literal_item->value, item_lc, arr);
    }
  } break;

  case SYNTAX_LIST_LITERAL: {
    if (!right->inferred_type->is_list_type) {
      report_error(
        right->syntax->loc,
        "Right side of list pattern matching is not a list type but '%s'",
        type_to_string(right->inferred_type));
      exit(1);
    }

    auto list_literal = syntax_assert_cast<Syntax_List_Literal>(left);

    if (list_literal->items.len == 0) {
      report_error(left->loc, "Specify at least one element in list pattern matching");
      exit(1);
    }

    AST *tail = right;

    FOR (list_literal->items) {
      Syntax *item = *it;
    // {
      AST *head_lc = alloc<AST>();
      head_lc->syntax = item;
      head_lc->inferred_type = right->inferred_type->list_item_type;
      head_lc->is_type_checking_done = true;
      head_lc->value_kind = VALUE_KIND_VALUE;
      head_lc->is_lvalue_ = !head_lc->inferred_type->is_primitive_type;
      head_lc->list = tail;

      head_lc->ir_generate_fn = [] (IR_Writer *w, AST *head_lc) {
        auto list_head = head_lc->list->ir_value_; // List is always pointer

        // Here should be check for empty list and possibly exception
        auto data_pointer = w->create_int_add(list_head, ir_create_immediate_i64(8));
        auto data = w->create_64bit_load(data_pointer);
        head_lc->ir_value_ = data;
        head_lc->is_lvalue_ = !head_lc->inferred_type->is_primitive_type;
      };

      arr->append(head_lc);
      traverse_pattern_matching(helper, parent_scope, item, head_lc, arr);

      AST *new_tail_lc = alloc<AST>();
      new_tail_lc->syntax = item;
      new_tail_lc->inferred_type = right->inferred_type;
      new_tail_lc->is_type_checking_done = true;
      new_tail_lc->value_kind = VALUE_KIND_VALUE;
      new_tail_lc->is_lvalue_ = false;
      new_tail_lc->list = tail;

      new_tail_lc->ir_generate_fn = [] (IR_Writer *w, AST *new_tail_lc) {
        auto list_head = new_tail_lc->list->ir_value_; // List is always pointer
        auto next = w->create_64bit_load(list_head);
        new_tail_lc->ir_value_ = next;
      };

      arr->append(new_tail_lc);
      tail = new_tail_lc;
    }

    if (list_literal->rest) {
      if (list_literal->rest->type != SYNTAX_IDENTIFIER) {
        report_error(left->loc, "List pattern matching only supports identifier as tail of the list");
        exit(1);
      }

      const char *name_of_the_tail = syntax_assert_cast<Syntax_Identifier>(list_literal->rest)->name;
      parent_scope->add(name_of_the_tail, tail);
    }
  } break;

  default: {
    report_error(left->loc, "Not supported in pattern matching(yet)");
    exit(1);
  } break;
  }
}


uint32_t/*offset*/ do_c_like_struct_sizing(uint32_t *struct_size, uint32_t *struct_alignment, uint32_t field_size, uint32_t field_alignment) {
  assert(field_size);
  assert(field_alignment);
  *struct_size = align(*struct_size, field_alignment);
  uint32_t offset = *struct_size;
  *struct_size = *struct_size + field_size;
  if (*struct_alignment < field_alignment) {
    *struct_alignment = field_alignment;
  }

  return offset;
}

uint32_t/*offset*/ do_c_like_struct_sizing(uint32_t *struct_size, uint32_t *struct_alignment, Type_Information *struct_field) {
  auto result = do_c_like_struct_sizing(struct_size, struct_alignment, struct_field->size, struct_field->alignment);
  return result;
}


void finish_c_like_struct_sizing(uint32_t *size, uint32_t max_alignment) {
  *size = align(*size, max_alignment);
}

Type_Information *resolve_tuple_type(AST *workspace, Array<Type_Information *> types, CodeLoc loc) {
  assert(types.len);

  Type_Information *found = NULL;

  FOR (workspace->tuple_types) {
    Type_Information *tuple_type = *it; if (tuple_type->tuple_items.len != types.len) continue;

    bool types_match = true;
    for (int i = 0; i < types.len; ++i) {
      auto type_from_existing = tuple_type->tuple_items[i].type;
      auto type_from_candidate = types[i];
      if (!types_are_equal(type_from_existing, type_from_candidate)) {
        types_match = false;
        break;
      }
    }

    if (types_match) {
      found = tuple_type;
      break;
    }
  }

  if (!found) {
    auto new_one = alloc<Type_Information>();
    new_one->loc = loc;
    new_one->is_tuple_type = true;

    Builder name_builder = {};
    name_builder.append_c_str("{");

    int index = 0;
    FOR (types) {
      Type_Information *type = *it;

      if (index != 0) {
        name_builder.append_c_str(", ");
      }
      name_builder.append_c_str(type_to_string(type));

      Tuple_Item item = {};
      item.type = type;
      item.offset = do_c_like_struct_sizing(&new_one->size, &new_one->alignment, type);
      new_one->tuple_items.append(item);

      index++;
    }
    finish_c_like_struct_sizing(&new_one->size, new_one->alignment);

    name_builder.append_c_str("}");
    name_builder.append_byte(0);

    new_one->name = name_builder.data;

    workspace->tuple_types.append(new_one);
    found = new_one;
    printf("New Tuple type %s -> %d\n", new_one->name, new_one->loc.offset0);
  }

  return found;
}

Type_Information *resolve_union_type(AST *workspace, Type_Information *left, Type_Information *right) {
  if (left == right) return left;

  Array<Type_Information *> left_array = {};
  if (left->is_union_type) {
    FOR (left->union_type_variants) {
      left_array.append(*it);
    }
  } else {
    left_array.append(left);
  }

  auto append_if_doesnot_exists = [&] (Type_Information *candidate) {
    for (int i = 0; i < left_array.len; ++i) {
      if (left_array[i] == candidate) return;
    }
    left_array.append(candidate);
  };

  if (right->is_union_type) {
    FOR (right->union_type_variants) {
      append_if_doesnot_exists(*it);
    }
  } else {
    append_if_doesnot_exists(right);
  }

  left_array.sort([](Type_Information *left, Type_Information *right) {
    return left < right;
  });

  Type_Information *found = NULL;

  FOR (workspace->union_types) {
    auto *union_type = *it;
    if (union_type->union_type_variants.len != left_array.len) {
      continue;
    }

    bool ok = true;
    for (int i = 0; i < union_type->union_type_variants.len; ++i) {
      auto variant_of_existing_union = union_type->union_type_variants[i];
      auto variant_of_candidate_union = left_array[i];
      if (!types_are_equal(variant_of_candidate_union, variant_of_existing_union)) {
        ok = false;
        break;
      }
    }

    if (ok) {
      found = union_type;
      break;
    }
  }

  if (!found) {
    auto new_one = alloc<Type_Information>();
    new_one->is_union_type = true;
    new_one->union_type_variants = left_array;

    // tag 64bit
    do_c_like_struct_sizing(&new_one->size, &new_one->alignment, workspace->builtin_types.i64);

    // common memory
    uint32_t common_size = 0;
    uint32_t common_alignment = 0;
    FOR (left_array) {
      auto variant = *it;
      if (variant->size > common_size) common_size = variant->size;
      if (variant->alignment > common_alignment) common_alignment = variant->alignment;
    }
    do_c_like_struct_sizing(&new_one->size, &new_one->alignment, common_size, common_alignment);
    finish_c_like_struct_sizing(&new_one->size, new_one->alignment);

    Builder name_builder = {};
    bool first = true;
    FOR (left_array) {
      auto variant = *it;
      if (!first) name_builder.append_c_str("|");
      name_builder.append_c_str(variant->name);
      first = false;
    }
    name_builder.append_byte('\0');
    new_one->name = name_builder.data;

    workspace->union_types.append(new_one);
    found = new_one;
  }

  return found;
}

Type_Information *resolve_atom_type(AST *workspace, const char *name) {
  Type_Information *found = NULL;
  FOR (workspace->atom_types) {
    auto existing_atom_type = *it;
    if (strcmp(existing_atom_type->name, name) == 0) {
      found = existing_atom_type;
      break;
    }
  }

  if (!found) {
    found = alloc<Type_Information>();
    found->name = name;
    found->is_atom_type = true;
    found->is_primitive_type = true;
    found->atom_index = workspace->atom_types.len;
    found->size = workspace->builtin_types.i64->size;
    found->alignment = workspace->builtin_types.i64->alignment;

    workspace->atom_types.append(found);
  }

  return found;
}

Type_Information *resolve_list_type(AST *workspace, Type_Information *item_type) {
  Type_Information *found = NULL;
  FOR (workspace->list_types) {
    auto existing_list_type = *it;
    if (existing_list_type->list_item_type == item_type) {
      found = existing_list_type;
      break;
    }
  }

  if (!found) {
    found = alloc<Type_Information>();
    found->name = alloc_sprintf("[%s]", item_type->name);
    found->is_list_type = true;
    found->is_primitive_type = true;
    found->list_item_type = item_type;
    found->size = workspace->builtin_types.rawptr->size;
    found->alignment = workspace->builtin_types.rawptr->alignment;

    workspace->list_types.append(found);
  }

  return found;
}

Type_Information polymorphic_type_stub = {
  .name = "<polymorphic_stub>",
  .size = 1,
  .alignment = 1,
};

Type_Information *resolve_polymorphic_type_stub(const char *name) {
  assert(name);
  auto type = alloc<Type_Information>();
  type->is_poly_stub = true;
  type->size = 1;
  type->alignment = 1;
  type->name = name;
  return type;
}


void init_function_lc_if_necessary(Syntax_Function *function, Scope *parent_scope) {
  if (!function->ast) {
    AST *function_lc = alloc<AST>();
    function_lc->syntax = function;
    function_lc->value_kind = VALUE_KIND_FUNCTION;
    function_lc->scope.parent_scope = parent_scope;

    function->ast = function_lc;
  }
}

AST *type_check_function(AST *workspace, Syntax_Function *function, Scope *parent_scope, AST *enclosing_function) {
  // for functions defined inside other functions
  init_function_lc_if_necessary(function, parent_scope);

  if (function->ast->is_type_checking_done) {
    return function->ast;
  }

  AST *function_lc = function->ast;
  function_lc->ir_function = function_lc->ir_function = workspace->ir_hub.create_function(function);
  function_lc->ir_value_ = function_lc->ir_function;
  if (function->name) {
    if (strcmp(function->name->name, "main") == 0) {
      function_lc->ir_function->entry_point = true;
    }
  }
  function_lc->belongs_to_function = enclosing_function;


  Type_Check_Helper new_helper_ = {};
  new_helper_.workspace = workspace;

  Type_Check_Helper *helper = &new_helper_;

  Array<Type_Information *> arg_types = {};

  Array<Syntax_Identifier *> unknown_identifiers_in_argument_types = {};

  if (function_lc->specialization_of_polymorphic_function) {
    FOR (function_lc->scope.parent_scope->entries) {
      auto pair = *it;
      printf("%s -> %p\n", pair.name, pair.ast);
    }
  }

  FOR (function->syntax_arg_groups) {
    Syntax_Function_Argument_Group *arg_group = *it;

    Type_Check_Params params = {};
    if (!function_lc->specialization_of_polymorphic_function) {
      params.should_save_possible_polymorphic_identifier_into = &unknown_identifiers_in_argument_types;
    }
    params.enclosing_function = function_lc;

    auto group_type_lc = typecheck(helper, arg_group->type_expr, &function_lc->scope, params);
    require_to_be_a_type(group_type_lc);

    FOR (arg_group->names) {
      Syntax_Identifier *name = *it;

      AST *new_arg = alloc<AST>();
      new_arg->syntax = name;
      new_arg->is_type_checking_done = true;
      new_arg->inferred_type = group_type_lc->inferred_type;
      new_arg->value_kind = VALUE_KIND_VALUE;
      new_arg->is_lvalue_ = !is_integral_type(new_arg->inferred_type);
      // new_arg->is_argument = true;
      // new_arg->argument_position = argument_count;
      new_arg->ir_value_ = function_lc->ir_function->create_argument();
      new_arg->ir_value_->is_floating_point = types_are_equal(new_arg->inferred_type, helper->workspace->builtin_types.f64);
      new_arg->belongs_to_function = function_lc;

      AST *existing = function_lc->scope.find(name->name, false/*do not traverse up*/);
      if (existing) {
        report_error(name->loc, "Identifier with such name is already defined, previously defined here");
        report_error(existing->syntax->loc, "Other definition");
        exit(1);
      }

      function_lc->scope.add(name->name, new_arg);
      arg_types.append(new_arg->inferred_type);
    }
  }

  if (unknown_identifiers_in_argument_types.len) {
    Array<Syntax_Identifier *> poly_params = {};
    FOR (unknown_identifiers_in_argument_types) {
      Syntax_Identifier *ident = *it;
      if (ident->name[0] == '$') {
        FOR (poly_params) {
          Syntax_Identifier *existing = *it;
          if (strcmp(existing->name, ident->name) == 0) {
            report_error(ident->loc, "Duplicate polymorphic type declaration");
            report_error(existing->loc, "Previous was here");
            exit(1);
          }
        }
        poly_params.append(ident); // skip '$' at the start
      }
    }

    FOR (unknown_identifiers_in_argument_types) {
      Syntax_Identifier *ident = *it;
      if (ident->name[0] != '$') {
        bool found = false;
        FOR (poly_params) {
          Syntax_Identifier *poly_param_ident = *it;
          if (strcmp(poly_param_ident->name + 1/*to skip $*/, ident->name) == 0) {
            found = true;
            break;
          }
        }
        if (!found) {
          // use it for regular path and cause unknown identifier error message
          typecheck(helper, ident, &function_lc->scope, {});
          assert(0 && "Unknown identifier should be reported but it was not");
        }
      }
    }

    FOR (poly_params) {
      Syntax_Identifier *poly_param_ident = *it;
      const char *name = poly_param_ident->name;
      function_lc->polymorphic_function_poly_params.append(name);
    }

    function_lc->value_kind = VALUE_KIND_POLYMORPHIC_FUNCTION;
    function_lc->inferred_type = resolve_polymorphic_type_stub("<poly_function_stub>");
    function_lc->is_type_checking_done = true;
    function_lc->polymorphic_function = function_lc;
    function_lc->polymorphic_function_arg_types = arg_types;
    return function_lc;
  }

  Type_Information *return_type = NULL;
  if (function->return_type_expr) {
    AST *return_type_expr_lc = typecheck(helper, function->return_type_expr, &function_lc->scope, {});
    require_to_be_a_type(return_type_expr_lc);
    return_type = return_type_expr_lc->inferred_type;
  }

  if (return_type) {
    function_lc->inferred_type = resolve_function_type(workspace, arg_types, return_type);
    function_lc->is_type_checking_done = true;
  }

  if (function->body) {
    Type_Check_Params new_params = {.last_statement = true, .enclosing_function = function_lc};

    IR_Instruction instruction_stub = {};
    IR_Writer ir_writer = {};
    ir_writer.workspace = workspace;
    ir_writer.hub = &workspace->ir_hub;
    ir_writer.scratchpad = &function_lc->ir_function->scratchpad;
    ir_writer.last_instruction = &instruction_stub;
    //TODO loc

    auto body_lc = typecheck(helper, function->body, &function_lc->scope, new_params);

    function_lc->body_lc = body_lc;
    require_to_be_a_value(body_lc);

    if (!return_type) {
      return_type = body_lc->inferred_type;
      function_lc->inferred_type = resolve_function_type(workspace, arg_types, return_type);
      function_lc->is_type_checking_done = true;
    }

    if (types_are_equal(return_type, workspace->builtin_types.void_)) {
      //ir_writer.create_ret(NULL);
    } else if (!types_are_equal(return_type, body_lc->inferred_type)) {
      report_error(function->body->statements.last()->loc, "Type does match return type of a function");
      report_error(function->return_type_expr->loc, "return type defined here");
      report_type_mismatch_message(return_type, body_lc->inferred_type);
      exit(1);
    } else {
      //IR_Value *result = NULL;
      //if (is_integral_type(return_type)) {
      //  result = get_rvalue(&ir_writer, body_lc);
      //} else {
      //  result = get_lvalue(body_lc);
      //}
      //ir_writer.create_ret(result);
    }

  }

  if (!function->return_type_expr && !function->body) {
    report_error(function->loc, "In a function either explicit return type should present and/or function body");
    exit(1);
  }

  if (types_are_equal(return_type, workspace->builtin_types.f64)) {
    function_lc->ir_function->returns_float = true;
  }

  if (function_lc->closure_capture_struct) {
    finish_c_like_struct_sizing(&function_lc->closure_capture_struct->size, function_lc->closure_capture_struct->size);
    //report_error(function->loc, "%s has capture struct size = %d", function_lc->ir_function->name, function_lc->closure_capture_struct->size);
  }

  assert(function_lc->value_kind == VALUE_KIND_FUNCTION);
  workspace->functions_to_generate.append(function_lc);

  return function_lc;
}

bool traverse_poly_matching(
  Type_Information *callee_side_type, Type_Information *caller_side_type,
  Scope *polymorphic_params) {

  if (types_are_equal(callee_side_type, caller_side_type)) {
    return true;
  }

  if (callee_side_type->is_poly_stub) {
    if (callee_side_type->name[0] == '$') {
      AST *type_lc = alloc<AST>();
      type_lc->is_type_checking_done = true;
      type_lc->value_kind = VALUE_KIND_TYPE;
      type_lc->inferred_type = caller_side_type;
      polymorphic_params->add(callee_side_type->name + 1, type_lc);
    }
  } else if (callee_side_type->is_tuple_type && caller_side_type->is_tuple_type) {
    if (callee_side_type->tuple_items.len != caller_side_type->tuple_items.len) {
      return false;
    }

    for (int i = 0; i < callee_side_type->tuple_items.len; ++i) {
      traverse_poly_matching(
        callee_side_type->tuple_items[i].type,
        caller_side_type->tuple_items[i].type,
        polymorphic_params);
    }
 } else if (callee_side_type->is_function_type && caller_side_type->is_function_type) {
   if (callee_side_type->function_type_arguments.len != caller_side_type->function_type_arguments.len) {
     return false;
   }
   for (int i = 0; i < callee_side_type->function_type_arguments.len; ++i) {
     traverse_poly_matching(
       callee_side_type->function_type_arguments[i],
       caller_side_type->function_type_arguments[i],
       polymorphic_params);
   }

   traverse_poly_matching(
     callee_side_type->function_type_return,
     caller_side_type->function_type_return,
     polymorphic_params);
  } else if (callee_side_type->is_union_type && caller_side_type->is_union_type) {
    if (callee_side_type->union_type_variants.len != caller_side_type->union_type_variants.len) {
      return false;
    }

    auto &wanted = callee_side_type->union_type_variants;
    auto &given  = caller_side_type->union_type_variants;

    Array<Type_Information *> common = {};

    for (int i = 0; i < wanted.len; i++) {
      for (int j = 0; j < given.len; j++) {
        if (wanted[i] == given[j]) {
          common.append(wanted[i]);
        }
      }
    }

    Array<Type_Information *> callee_diff = {};
    Array<Type_Information *> caller_diff = {};
    FOR (wanted) {
      auto *wanted_type = *it;
      bool found = false;
      FOR (common) {
        auto *common_type = *it;
        if (wanted_type == common_type) {
          found = true;
          break;
        }
      }

      if (!found) callee_diff.append(wanted_type);
    }

    FOR (given) {
      auto *given_type = *it;
      bool found = false;
      FOR (common) {
        auto *common_type = *it;
        if (given_type == common_type) {
          found = true;
          break;
        }
      }

      if (!found) caller_diff.append(given_type);
    }

    callee_diff.sort([](auto a, auto b) { return a < b; });
    caller_diff.sort([](auto a, auto b) { return a < b; });

    for (int i = 0; i < callee_diff.len; i++) {
      printf("Union matching '%s' <-> '%s'\n",
             type_to_string(callee_diff[i]),
             type_to_string(caller_diff[i])
             );
      auto success = traverse_poly_matching(callee_diff[i], caller_diff[i], polymorphic_params);
      if (!success) return false;
    }
  } else if (callee_side_type->is_union_type && !caller_side_type->is_union_type) {
    Array<Type_Information *> found_poly_types = {};
    FOR (callee_side_type->union_type_variants) {
      auto variant = *it;
      if (variant->is_poly_stub) {
        found_poly_types.append(variant);
      }
    }

    if (found_poly_types.len == 1) {
      auto success = traverse_poly_matching(found_poly_types[0], caller_side_type, polymorphic_params);
      if (!success) return false;
    }
  } else if (callee_side_type->is_list_type && caller_side_type->is_list_type) {
      auto success = traverse_poly_matching(callee_side_type->list_item_type, caller_side_type->list_item_type, polymorphic_params);
      if (!success) return false;
  }

  return true;
}

AST *find_or_create_specialization(Type_Check_Helper *helper, AST *polymorphic_function, Array<Type_Information *> call_args, Syntax_Call *call) {
  assert(polymorphic_function->value_kind == VALUE_KIND_POLYMORPHIC_FUNCTION);

  if (polymorphic_function->polymorphic_function_arg_types.len != call_args.len) {
    report_error(call->loc, "Function arguments count mismatch");
    exit(1);
  }

  AST *found = NULL;
  FOR (polymorphic_function->function_specializations) {
    FunctionSpecializationPair &existing = *it;
    if (existing.call_args.len != call_args.len) continue;

    bool ok = true;
    for (int i = 0; i < existing.call_args.len; ++i) {
      if (existing.call_args[i] != call_args[i]) {
        ok = false;
        break;
      }
    }

    if (!ok) continue;

    found = existing.specialization;
    break;
  }

  if (!found) {
    Syntax_Function *ast_function = syntax_assert_cast<Syntax_Function>(polymorphic_function->syntax);
    Scope *polymorphic_params_scope = alloc<Scope>();
    polymorphic_params_scope->parent_scope = polymorphic_function->scope.parent_scope;

    for (int i = 0; i < polymorphic_function->polymorphic_function_arg_types.len; i++) {
      auto callee_type = polymorphic_function->polymorphic_function_arg_types[i];
      auto given_type = call_args[i];
      bool success = traverse_poly_matching(callee_type, given_type, polymorphic_params_scope);
      if (!success) {
        report_error(call->args[i]->loc, "Function call argument #%d mismatch", i + 1);
        report_type_mismatch_message(callee_type, given_type);
        exit(1);
      }
    }

    FOR (polymorphic_function->polymorphic_function_poly_params) {
      const char *name = *it;
      if (!polymorphic_params_scope->find(name + 1/*to skip $*/, false/*do not traverse up*/)) {
        report_error(call->loc, "Was not able to resolve '%s'", name);
        report_error(polymorphic_function->syntax->loc, "Polymorphic function was defined here");
        exit(1);
      }
    }

    Syntax_Function *function_shallow_copy = syntax_new<Syntax_Function>();
    *function_shallow_copy = *ast_function;

    function_shallow_copy->ast = alloc<AST>();
    function_shallow_copy->ast->value_kind = VALUE_KIND_FUNCTION;
    function_shallow_copy->ast->specialization_of_polymorphic_function = polymorphic_function;
    function_shallow_copy->ast->scope.parent_scope = polymorphic_params_scope;

    // So generate_ir recursive traversing does not crash when specialization is encountered in call
    function_shallow_copy->ast->ir_generate_fn = [] (IR_Writer *w, AST *lc) { };

    polymorphic_function->function_specializations.append({call_args, function_shallow_copy->ast});

    found = type_check_function(helper->workspace, function_shallow_copy, NULL, NULL);
  }

  return found;
}

uint32_t/*offset*/ add_capture_to_function(AST *function, const char *name, Type_Information *value_type) {
  assert(function->value_kind == VALUE_KIND_FUNCTION);
  if (!function->closure_capture_struct) {
    auto closure_struct = alloc<Type_Information>();
    closure_struct->is_struct_type = true;
    function->closure_capture_struct = closure_struct;
  }

  Struct_Field field = {};
  if (!find_struct_field(function->closure_capture_struct, name, &field)) {
    field.name = name;
    field.type = value_type;
    field.offset = do_c_like_struct_sizing(
      &function->closure_capture_struct->size,
      &function->closure_capture_struct->alignment,
      field.type);
    function->closure_capture_struct->struct_fields.append(field);
  }

  return field.offset;
}

AST *typecheck(Type_Check_Helper *helper, Syntax *node, Scope *parent_scope, Type_Check_Params params) {
  auto is_last_statement = params.last_statement;
  params.last_statement = false; // do not proxy it further automatically

  Type_Information *wanted_type = params.wanted_type;
  params.wanted_type = NULL; // do not proxy it further automatically

  AST *result_lc = alloc<AST>();
  result_lc->syntax = node;

  switch (node->type) {
    case SYNTAX_FUNCTION: {
      auto function = syntax_assert_cast<Syntax_Function>(node);
      auto function_lc = type_check_function(helper->workspace, function, parent_scope, params.enclosing_function);

      result_lc = function_lc;
    } break;

    case SYNTAX_BLOCK: {
      auto block = syntax_assert_cast<Syntax_Block>(node);

      Scope *scope_of_block = alloc<Scope>();
      scope_of_block->parent_scope = parent_scope;

      AST *last_statement_lc = NULL;

      for (int i = 0; i < block->statements.len; i++) {
        Syntax *statement = block->statements[i];
        auto new_params = params;
        if (is_last_statement) {
          if (i == block->statements.len - 1) {
            new_params.last_statement = true;
          } else {
            new_params.last_statement = false;
          }
        }
        auto statement_lc = typecheck(helper, statement, scope_of_block, new_params);
        last_statement_lc = statement_lc;

        result_lc->block_statements.append(statement_lc);
      }

      if (block->statements.len) {
        copy_value(result_lc, last_statement_lc);
      } else {
        result_lc->inferred_type = helper->workspace->builtin_types.void_;
        result_lc->value_kind = VALUE_KIND_VALUE;
      }

      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        AST *last_statement_lc = NULL;
        FOR (lc->block_statements) {
          generate_ir(w, *it);
          last_statement_lc = *it;
        }
        if (last_statement_lc) {
          lc->ir_value_ = last_statement_lc->ir_value_;
        }
      };
    } break;

    case SYNTAX_STRING_LITERAL: {
      result_lc->inferred_type = helper->workspace->builtin_types.string;
      result_lc->value_kind = VALUE_KIND_VALUE;
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        auto literal = syntax_assert_cast<Syntax_String_Literal>(lc->syntax);
        lc->ir_value_ = w->hub->create_const_string_struct(literal->value, strlen(literal->value));
      };
      set_value(result_lc, NULL /*ir_value*/, true);
    } break;

    case SYNTAX_NUMBER_LITERAL: {
      auto literal = syntax_assert_cast<Syntax_Number_Literal>(node);

      bool dot_found = false;
      auto it = literal->value;
      while (*it) {
        if (*it == '.') {
          dot_found = true;
        }
        it++;
      }
      IR_Value *value = NULL;
      if (dot_found) {
        result_lc->inferred_type = helper->workspace->builtin_types.f64;
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          auto literal = syntax_assert_cast<Syntax_Number_Literal>(lc->syntax);
          lc->ir_value_ =  ir_create_immediate_f64(strtod(literal->value, NULL));
        };
      } else {
        result_lc->inferred_type = helper->workspace->builtin_types.i64;
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          auto literal = syntax_assert_cast<Syntax_Number_Literal>(lc->syntax);
          lc->ir_value_ =  ir_create_immediate_i64(atoll(literal->value));
        };
      }
      set_value(result_lc, value, false);
    } break;

    case SYNTAX_UNARY_OP: {
      auto unary_op = syntax_assert_cast<Syntax_Unary_Op>(node);
      auto operand_lc = typecheck(helper, unary_op->operand, parent_scope, params);
      require_to_be_a_value(operand_lc);


      if (operand_lc->inferred_type == helper->workspace->builtin_types.i64) {
        switch(unary_op->op) {
        case UNARY_OP_PLUS: {
          result_lc = operand_lc;
        } break;

        case UNARY_OP_MINUS: {
          result_lc->unary_op_operand_lc = operand_lc;
          result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
            generate_ir(w, lc->unary_op_operand_lc);
            IR_Value *operand_rvalue = get_rvalue(w, lc->unary_op_operand_lc);
            auto minus_one = ir_create_immediate_i64(-1);
            lc->ir_value_ = w->create_int_mul(operand_rvalue, minus_one);
          };

          result_lc->inferred_type = operand_lc->inferred_type;
          set_value(result_lc, NULL/* ir_result */, false);
        } break;

        case UNARY_OP_NOOP:
        case UNARY_OP_DEREFERENCE:
        case UNARY_OP_ADDRESSOF:
        case UNARY_OP_LOGICAL_NEGATE:
        case UNARY_OP_BITWISE_NEGATE: {
          assert(0 && "binary operator not implemented");

        } break;
        }
      } else if (operand_lc->inferred_type == helper->workspace->builtin_types.f64) {
        switch(unary_op->op) {
        case UNARY_OP_PLUS: {
          result_lc = operand_lc;
        } break;

        case UNARY_OP_MINUS: {
          result_lc->unary_op_operand_lc = operand_lc;
          result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
            generate_ir(w, lc->unary_op_operand_lc);
            IR_Value *operand_rvalue = get_rvalue(w, lc->unary_op_operand_lc);
            auto minus_one = ir_create_immediate_f64(-1);
            lc->ir_value_ = w->create_two_operand_instruction(IR_INSTRUCTION_FLOAT_MUL, operand_rvalue, minus_one);
          };

          result_lc->inferred_type = operand_lc->inferred_type;
          set_value(result_lc, NULL/*ir_result*/, false);
        } break;

        case UNARY_OP_NOOP:
        case UNARY_OP_DEREFERENCE:
        case UNARY_OP_ADDRESSOF:
        case UNARY_OP_LOGICAL_NEGATE:
        case UNARY_OP_BITWISE_NEGATE: {
          assert(0 && "binary operator not implemented");

        } break;
        }
      } else if (operand_lc->inferred_type == helper->workspace->builtin_types.bool_) {
        switch(unary_op->op) {
        case UNARY_OP_LOGICAL_NEGATE: {
          result_lc->inferred_type = operand_lc->inferred_type;
          result_lc->unary_op_operand_lc = operand_lc;
          result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
            auto operand_lc = lc->unary_op_operand_lc;
            generate_ir(w, operand_lc);
            IR_Value *operand_rvalue = get_rvalue(w, operand_lc);
            lc->ir_value_ = w->create_one_operand_instruction(IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT, operand_rvalue);
          };
          set_value(result_lc, NULL/*ir_result*/, false);
        } break;
        case UNARY_OP_PLUS:
        case UNARY_OP_MINUS:
        case UNARY_OP_NOOP:
        case UNARY_OP_DEREFERENCE:
        case UNARY_OP_ADDRESSOF:
        case UNARY_OP_BITWISE_NEGATE: {
          assert(0 && "binary operator not implemented");

        } break;
        }
      } else if (operand_lc->inferred_type->is_list_type && unary_op->op == UNARY_OP_LOGICAL_NEGATE) {
          result_lc->inferred_type = helper->workspace->builtin_types.bool_;
          result_lc->unary_op_operand_lc = operand_lc;
          result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
            auto operand_lc = lc->unary_op_operand_lc;
            generate_ir(w, operand_lc);
            IR_Value *operand_rvalue = get_rvalue(w, operand_lc);
            lc->ir_value_ = w->create_one_operand_instruction(IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT, operand_rvalue);
          };
          set_value(result_lc, NULL/*ir_result*/, false);
      } else {
        report_error(unary_op->loc, "Unary operations is not allowed on this type: %s", type_to_string(operand_lc->inferred_type));
        exit(1);
      }
    } break;
    //
    case SYNTAX_BINARY_OP: {
      auto binary_op = syntax_assert_cast<Syntax_Binary_Op>(node);
      auto left_lc = typecheck(helper, binary_op->left, parent_scope, params);
      auto right_lc = typecheck(helper, binary_op->right, parent_scope, params);

      if (is_a_type(left_lc) && is_a_type(right_lc) && binary_op->op == BINARY_OP_BITWISE_OR) {
        result_lc->value_kind = VALUE_KIND_TYPE;
        result_lc->inferred_type = resolve_union_type(helper->workspace, left_lc->inferred_type, right_lc->inferred_type);
      } else {
        require_to_be_a_value(left_lc);
        require_to_be_a_value(right_lc);

        Type_Information *result_type = NULL;
        IR_Value *ir_result = NULL;

        auto do_regular_handling = [&] (IR_Instruction_Type instruction_type, Type_Information *data_type) {
          result_type = data_type;
        };

        if (types_are_equal(left_lc->inferred_type, helper->workspace->builtin_types.i64)) {
          auto left_type = left_lc->inferred_type;
          auto bool_type = helper->workspace->builtin_types.bool_;
          switch(binary_op->op) {
          case BINARY_OP_PLUS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_ADD, left_type); break;
          case BINARY_OP_MINUS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_SUB, left_type); break;
          case BINARY_OP_MULTIPLY: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_MUL, left_type); break;
          case BINARY_OP_DIVISION: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_DIV, left_type); break;
          case BINARY_OP_REMAINDER: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_REMAINDER, left_type); break;

          case BINARY_OP_BITWISE_OR: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_OR, left_type); break;
          case BINARY_OP_BITWISE_AND: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_AND, left_type); break;
          case BINARY_OP_BITWISE_XOR: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR, left_type); break;
          case BINARY_OP_BITWISE_SHIFT_LEFT: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT, left_type); break;
          case BINARY_OP_BITWISE_SHIFT_RIGHT: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT, left_type); break;


          case BINARY_OP_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL, bool_type); break;
          case BINARY_OP_NOT_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL, bool_type); break;
          case BINARY_OP_LESS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_LESS, bool_type); break;
          case BINARY_OP_LESS_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL, bool_type); break;
          case BINARY_OP_GREATER: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER, bool_type); break;
          case BINARY_OP_GREATER_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL, bool_type); break;

          default:
            report_error(binary_op->op_loc, "Operation is not allowed on i64");
            exit(1);
          }

          //assert(ir_result);
          set_value(result_lc, ir_result, false);
          result_lc->inferred_type = result_type;

          result_lc->binary_op_left_operand_lc = left_lc;
          result_lc->binary_op_right_operand_lc = right_lc;
          result_lc->ir_generate_fn = generate_binary_op_ir_for_ints;

        } else if (types_are_equal(left_lc->inferred_type, helper->workspace->builtin_types.bool_)) {
          switch (binary_op->op) {
            // Do short circuiting
            case BINARY_OP_LOGICAL_AND: {
              result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
                generate_short_circuited_logical_operation(w, lc, 0);
              };
              //do_short_circuiting(0);
            } break;

            case BINARY_OP_LOGICAL_OR: {
              result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
                generate_short_circuited_logical_operation(w, lc, 1);
              };
              //do_short_circuiting(1);
            } break;

            default: {
              report_error(binary_op->op_loc, "Operation is not allowed on bool");
              exit(1);
            } break;
          }

          result_lc->binary_op_left_operand_lc = left_lc;
          result_lc->binary_op_right_operand_lc = right_lc;

          set_value(result_lc, NULL/*ir_result*/, false);
          result_lc->inferred_type = helper->workspace->builtin_types.bool_;
        } else if (types_are_equal(left_lc->inferred_type, helper->workspace->builtin_types.f64)) {
          auto left_type = left_lc->inferred_type;
          auto bool_type = helper->workspace->builtin_types.bool_;
          switch(binary_op->op) {
          case BINARY_OP_PLUS: do_regular_handling(IR_INSTRUCTION_FLOAT_ADD, left_type); break;
          case BINARY_OP_MINUS: do_regular_handling(IR_INSTRUCTION_FLOAT_SUB, left_type); break;
          case BINARY_OP_MULTIPLY: do_regular_handling(IR_INSTRUCTION_FLOAT_MUL, left_type); break;
          case BINARY_OP_DIVISION: do_regular_handling(IR_INSTRUCTION_FLOAT_DIV, left_type); break;

          case BINARY_OP_LESS: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_LESS, bool_type); break;
          case BINARY_OP_LESS_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL, bool_type); break;
          case BINARY_OP_GREATER: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_GREATER, bool_type); break;
          case BINARY_OP_GREATER_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL, bool_type); break;
          case BINARY_OP_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_EQUAL, bool_type); break;
          case BINARY_OP_NOT_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL, bool_type); break;

          case BINARY_OP_REMAINDER:

          case BINARY_OP_BITWISE_OR:
          case BINARY_OP_BITWISE_AND:
          case BINARY_OP_BITWISE_XOR:
          case BINARY_OP_LOGICAL_OR:
          case BINARY_OP_LOGICAL_AND:
          case BINARY_OP_BITWISE_SHIFT_LEFT:
          case BINARY_OP_BITWISE_SHIFT_RIGHT:
          case BINARY_OP_NOOP:
            report_error(binary_op->op_loc, "Binary operation is not allowed on floating point numbers"); // TODO ErrorReporting
            exit(1);
            break;
          }

          set_value(result_lc, NULL, false);
          result_lc->inferred_type = result_type;

          result_lc->binary_op_left_operand_lc = left_lc;
          result_lc->binary_op_right_operand_lc = right_lc;
          result_lc->ir_generate_fn = generate_binary_op_ir_for_floats;
        } else if (types_are_equal(left_lc->inferred_type, helper->workspace->builtin_types.string)){
          result_lc->binary_op_left_operand_lc = left_lc;
          result_lc->binary_op_right_operand_lc = right_lc;

#define DO_STRING_COMPARISON(INSTRUCTION_TYPE) \
  set_value(result_lc, ir_result, false); \
  result_lc->inferred_type = helper->workspace->builtin_types.bool_; \
  result_lc->ir_generate_fn = [] (IR_Writer *w, AST *ast) { \
    generate_string_comparison_ir(w, ast, INSTRUCTION_TYPE); \
  }

          if (binary_op->op == BINARY_OP_PLUS) {
            result_lc->inferred_type = helper->workspace->builtin_types.string;
            result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
              generate_ir(w, lc->binary_op_left_operand_lc);
              generate_ir(w, lc->binary_op_right_operand_lc);

              Array<IR_Value *> args = {};
              args.append(get_lvalue(w, lc->binary_op_left_operand_lc));
              args.append(get_lvalue(w, lc->binary_op_right_operand_lc));

              lc->ir_value_ = w->create_call(
                w->workspace->support_library_functions.string_concat,
                args, NULL);
            };


            set_value(result_lc, NULL/*ir_result*/, true);
          } else if (binary_op->op == BINARY_OP_EQUAL) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL);
          } else if (binary_op->op == BINARY_OP_NOT_EQUAL) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL);
          } else if (binary_op->op == BINARY_OP_LESS) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_LESS);
          } else if (binary_op->op == BINARY_OP_LESS_OR_EQUAL) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL);
          } else if (binary_op->op == BINARY_OP_GREATER) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER);
          } else if (binary_op->op == BINARY_OP_GREATER_OR_EQUAL) {
            DO_STRING_COMPARISON(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL);
          } else {
            report_error(binary_op->op_loc, "Binary operation is not supported on strings"); // TODO ErrorReporting
            exit(1);
          }
#undef DO_STRING_COMPARISON
        } else {
          report_error(binary_op->op_loc, "Binary operation is not supported"); // TODO ErrorReporting
          exit(1);
        }
      }

      assert(result_lc->inferred_type);
    } break;

    case SYNTAX_IDENTIFIER: {
      Syntax_Identifier *ident = syntax_assert_cast<Syntax_Identifier>(node);
      assert(ident->name);

      const char *name = ident->name;

      if (name[0] == '$') {
        if (params.enclosing_function->specialization_of_polymorphic_function) {
          name++; // skip '$'
        }
      }

      AST *target = parent_scope->find(name);
      if (target) {
        if (!target->is_type_checking_done) {
          Type_Check_Helper new_helper = {};
          new_helper.workspace = helper->workspace;
          /*Type_Check_Result type_check_result =*/ typecheck(&new_helper, target->syntax, NULL, {});
          assert(target->is_type_checking_done);
          assert(target->inferred_type);
        }

        copy_value(result_lc, target);
      } else {
        if (params.should_save_possible_polymorphic_identifier_into) {
          params.should_save_possible_polymorphic_identifier_into->append(ident);
          result_lc->value_kind = VALUE_KIND_POLYMORPHIC_VALUE;
          result_lc->inferred_type = resolve_polymorphic_type_stub(ident->name);
          goto done;
        } else {
          report_error(ident->loc, "Unknown identifier");
          exit(1);
        }
      }

      if (is_a_value(target) && params.enclosing_function
          && target->belongs_to_function
          && params.enclosing_function != target->belongs_to_function) {

        for (auto it = params.enclosing_function->belongs_to_function;
             it != target->belongs_to_function; it = it->belongs_to_function) {

          add_capture_to_function(it, name, target->inferred_type);

          AST *captured_lc = alloc<AST>();
          *captured_lc = *target;
          captured_lc->capture_name = name;
          captured_lc->capture_enclosing_function = it;
          captured_lc->ir_generate_fn = generate_capture_ir;
          it->captures.append(captured_lc);

          set_value(captured_lc, NULL, true);

          it->scope.add(name, captured_lc);
        }

        add_capture_to_function(params.enclosing_function, name, target->inferred_type);

        result_lc->inferred_type = target->inferred_type;

        set_value(result_lc, NULL/*ir_result*/, true);

        AST *capture_lc = alloc<AST>();
        *capture_lc = *target;
        capture_lc->capture_name = name;
        capture_lc->capture_enclosing_function = params.enclosing_function;
        capture_lc->ir_generate_fn = generate_capture_ir;

        params.enclosing_function->captures.append(capture_lc);
        target = capture_lc;
      }

      result_lc->identifier_refers_to = target;
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        assert(lc->identifier_refers_to->ir_value_);
        lc->ir_value_ = lc->identifier_refers_to->ir_value_;
        //lc->is_lvalue_ = lc->identifier_refers_to->is_lvalue_;
      };
    } break;

    case SYNTAX_CALL: {
      auto call = syntax_assert_cast<Syntax_Call>(node);

      Array<AST*> arg_lcs = {};
      Array<Type_Information *> arg_types = {};
      FOR (call->args) {
        Syntax *arg = *it;

        auto arg_lc = typecheck(helper, arg, parent_scope, params);
        require_to_be_a_value(arg_lc);

        arg_lcs.append(arg_lc);
        arg_types.append(arg_lc->inferred_type);
      }

      auto callee_lc = typecheck(helper, call->callee, parent_scope, params);

      if (callee_lc->value_kind == VALUE_KIND_POLYMORPHIC_FUNCTION) {
        AST *specialization = find_or_create_specialization(helper, callee_lc->polymorphic_function, arg_types, call);
        callee_lc = specialization;
      }

      if (!callee_lc->inferred_type->is_function_type) {
        report_error(call->callee->loc, "Attempt to call not a function but value of type '%s'", type_to_string(callee_lc->inferred_type));
        exit(1);
      }

      Type_Information *callee_ft = callee_lc->inferred_type;
      assert(callee_ft->is_function_type);
      if (callee_ft->function_type_arguments.len != call->args.len) {
        report_error(call->loc, "Function arguments count mismatch");
        exit(1);
      }

      for (int i = 0; i < callee_ft->function_type_arguments.len; ++i) {
        Type_Information *left_type = callee_ft->function_type_arguments[i];
        Type_Information *arg_type = arg_types[i];
        AST *arg_lc = arg_lcs[i];

        if (left_type->is_union_type && !types_are_equal(left_type, arg_type)) {
          AST *promoted = {};
          if (emit_promotion_to_union(arg_lc, left_type, &promoted)) {
            promoted->inferred_type = left_type;
            promoted->is_type_checking_done = true;
            arg_lc = arg_lcs[i] = promoted;
            arg_type = arg_types[i] = left_type;
          }
        }

        if (!types_are_equal(left_type, arg_type)) {
          report_error(call->args[i]->loc, "Function call argument #%d mismatch", i + 1);
          report_type_mismatch_message(left_type, arg_type);
          exit(1);
        }
      }

      assert(callee_ft->function_type_return);
      result_lc->inferred_type = callee_ft->function_type_return;
      result_lc->value_kind = VALUE_KIND_VALUE;

      IR_Value *ir_result = NULL;
      bool is_tail_call = is_last_statement && callee_lc->value_kind == VALUE_KIND_FUNCTION && callee_lc->identifier_refers_to == params.enclosing_function;

      set_value(result_lc, ir_result, !is_integral_type(result_lc->inferred_type));
      result_lc->call_callee = callee_lc;
      result_lc->call_args = arg_lcs;
      result_lc->call_is_tail_call = is_tail_call;
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        Array<IR_Value *> arg_ir_values = {};
        FOR (lc->call_args) {
          generate_ir(w, *it);

          IR_Value *arg_ir_value = NULL;
          if (is_integral_type((*it)->inferred_type)) {
            arg_ir_value = get_rvalue(w, *it);
          } else {
            arg_ir_value = get_lvalue(w, *it);
          }
          arg_ir_values.append(arg_ir_value);
        }
        generate_ir(w, lc->call_callee);

        bool call_via_fat_pointer = lc->call_callee->value_kind != VALUE_KIND_FUNCTION;
        IR_Value *ir_callee = lc->call_callee->ir_value_;
        IR_Value *closure_base_address = NULL;
        if (call_via_fat_pointer) {
          closure_base_address = w->create_64bit_load(
            w->create_int_add(ir_callee, ir_create_immediate_i64(w->workspace->builtin_types.rawptr->size)));
          ir_callee = w->create_64bit_load(ir_callee);
        }

        if (lc->call_is_tail_call) {
          lc->ir_value_ = w->create_tail_call(ir_callee, arg_ir_values, closure_base_address);
        } else {
          lc->ir_value_ = w->create_call(ir_callee, arg_ir_values, closure_base_address);
        }
      };
    } break;

    case SYNTAX_SYSCALL: {
      auto syscall = syntax_assert_cast<Syntax_Syscall>(node);
      if (syscall->args.len == 0) {
        report_error(syscall->loc, "Syscall should have at least one argument (syscall number)");
        exit(1);
      }

      Array<AST *> args = {};

      auto arg0_lc = typecheck(helper, syscall->args[0], parent_scope, params);
      args.append(arg0_lc);

      if (!types_are_equal(arg0_lc->inferred_type, helper->workspace->builtin_types.i64)) {
        report_error(syscall->args[0]->loc, "Syscall number should be an integer number");
        report_type_mismatch_message(helper->workspace->builtin_types.i64, arg0_lc->inferred_type);
        exit(1);
      }


      for (int i = 1; i < syscall->args.len; ++i) {
        Syntax *arg = syscall->args[i];
        auto arg_lc = typecheck(helper, arg, parent_scope, params);
        args.append(arg_lc);

        require_to_be_a_value(arg_lc);
        if (!types_are_equal(arg_lc->inferred_type, helper->workspace->builtin_types.i64) &&
            !types_are_equal(arg_lc->inferred_type, helper->workspace->builtin_types.rawptr) ) {
          report_error(arg->loc, "Syscall argument should be an i64 or rawptr");
          report_error({}, "Given '%s'", type_to_string(arg_lc->inferred_type));
          exit(1);
        }

      }

      result_lc->inferred_type = helper->workspace->builtin_types.i64;
      result_lc->value_kind = VALUE_KIND_VALUE;

      set_value(result_lc, NULL/* ir_result */, false);

      result_lc->call_args = args;
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        Array<IR_Value *> arg_ir_values = {};
        FOR (lc->call_args) {
          generate_ir(w, *it);
          IR_Value *arg_ir_rvalue = get_rvalue(w, *it);
          arg_ir_values.append(arg_ir_rvalue);
        }

        lc->ir_value_ = w->create_syscall(arg_ir_values);
      };
    } break;

    case SYNTAX_STRUCT_DECLARATION: {
      auto struct_ = syntax_assert_cast<Syntax_Struct_Declaration>(node);

      Type_Information *struct_type_info = alloc<Type_Information>();
      struct_type_info->name = struct_->name->name;
      struct_type_info->loc = struct_->loc;
      struct_type_info->is_struct_type = true;

      FOR(struct_->syntax_field_groups) {
        auto syntax_field_group = *it;
        auto group_type_expr_lc = typecheck(
            helper, syntax_field_group->type_expr, &struct_->ast->scope, {});

        require_to_be_a_type(group_type_expr_lc);

        FOR(syntax_field_group->names) {
          auto ident = *it;

          int offset = do_c_like_struct_sizing(
              &struct_type_info->size, &struct_type_info->alignment,
              group_type_expr_lc->inferred_type);

          struct_type_info->struct_fields.append({
            .type = group_type_expr_lc->inferred_type,
            .name = ident->name,
            .offset = offset,
            .loc = ident->loc,
          });
        }
      }

      finish_c_like_struct_sizing(&struct_type_info->size, struct_type_info->alignment);

      // Find duplicate fiels
      for (int i = 0; i < struct_type_info->struct_fields.len; ++i) {
        auto &earlier_field = struct_type_info->struct_fields[i];
        for (int j = i + 1; j < struct_type_info->struct_fields.len; ++j) {
          auto &later_field = struct_type_info->struct_fields[j];
          if (strcmp(earlier_field.name, later_field.name) == 0) {
            report_error(later_field.loc, "Identifier with such name is already defined, previously defined here");
            report_error(earlier_field.loc, "Other definition");
            exit(1);
          }
        }
      }

      struct_->ast->inferred_type = struct_type_info;
      struct_->ast->value_kind = VALUE_KIND_TYPE;
      struct_->ast->is_type_checking_done = true;

      result_lc = struct_->ast;
    } break;

    case SYNTAX_STRUCT_LITERAL: {
      auto *literal = syntax_assert_cast<Syntax_Struct_Literal>(node);

      Type_Information *struct_type = NULL;

      if (literal->name) {
        auto literal_struct_name_lc = typecheck(helper, literal->name, parent_scope, params);
        require_to_be_a_type(literal_struct_name_lc);

        if (!literal_struct_name_lc->inferred_type->is_struct_type) {
          report_error(literal->name->loc, "Struct literal type does not refer to a struct");
          exit(1);
        }

        struct_type = literal_struct_name_lc->inferred_type;
      }

      if (literal->existing_value) {
        auto existing_value_lc = typecheck(helper, literal->existing_value, parent_scope, params);
        require_to_be_a_value(existing_value_lc);

        result_lc->struct_literal_existing_value = existing_value_lc;

        if (struct_type) {
          if (!types_are_equal(struct_type, existing_value_lc->inferred_type)) {
            report_error(literal->existing_value->loc, "Type of existing value provided to struct literal does not match type of struct literal");
            report_error(literal->loc, "Struct literal was defined here");
            report_type_mismatch_message(struct_type, existing_value_lc->inferred_type);
            exit(1);
          }
        } else {
          struct_type = existing_value_lc->inferred_type;
        }
      }

      if (!struct_type) {
        report_error(literal->loc, "Type nor existing value was provided in struct literal");
        exit(1);
      }

      FOR (literal->fields) {
        Syntax_Struct_Literal_Field *literal_field = *it;

        Struct_Field struct_field = {};
        if (!find_struct_field(struct_type, literal_field->name->name, &struct_field)) {
          report_error(literal_field->name->loc, "Field was not found at struct");
          report_error(struct_type->loc, "Struct was defined here");
          exit(1);
        }

        if (literal_field->optional_type_expr) {
          auto optional_type_expr_lc
            = typecheck(helper, literal_field->optional_type_expr, parent_scope, params);
          require_to_be_a_type(optional_type_expr_lc);

          if (!types_are_equal(struct_field.type, optional_type_expr_lc->inferred_type)) {
            report_error(literal_field->optional_type_expr->loc, "Explicit type does not match struct field type");
            report_error(struct_field.loc, "defined here");
            report_type_mismatch_message(struct_field.type, optional_type_expr_lc->inferred_type);
            exit(1);
          }
        }

        if (!literal_field->value) {
          report_error(literal_field->loc, "Value of struct literal field should be specified");
          exit(1);
        }

        auto params_copy = params;
        params_copy.wanted_type = struct_field.type;

        auto field_value_lc = typecheck(helper, literal_field->value, parent_scope, params_copy);
        require_to_be_a_value(field_value_lc);

        if (!types_are_equal(struct_field.type, field_value_lc->inferred_type)) {
          report_error(literal_field->value->loc, "Type of expression does not match struct field type");
          report_error(struct_field.loc, "defined here");
          report_type_mismatch_message(struct_field.type, field_value_lc->inferred_type);
          exit(1);
        }

        result_lc->struct_literal_entries.append({literal_field->name->name, field_value_lc});
      }

      result_lc->inferred_type = struct_type;
      set_value(result_lc, NULL/*address*/, true);
      result_lc->ir_generate_fn = generate_struct_literal_ir;
    } break;

    case SYNTAX_MEMBER_ACCESS: {
      auto *ma = syntax_assert_cast<Syntax_Member_Access>(node);
      auto value_lc = typecheck(helper, ma->value, parent_scope, params);

      if (!value_lc->inferred_type->is_struct_type) {
        report_error(ma->field->loc, "Member access only works on structs");
        exit(1);
      }
      require_to_be_a_value(value_lc);

      auto struct_ = value_lc->inferred_type;
      assert(struct_);


      Struct_Field field = {};
      if (!find_struct_field(struct_, ma->field->name, &field)) {
        report_error(ma->field->loc, "Field was not found at struct");
        report_error(struct_->loc, "Struct was defined here");
        exit(1);
      }

      result_lc->inferred_type = field.type;
      result_lc->value_kind = VALUE_KIND_VALUE;

      result_lc->member_access_struct = value_lc;
      result_lc->member_access_field_name = ma->field->name;
      result_lc->ir_generate_fn = generate_struct_member_access_ir;

      set_value(result_lc, NULL /* computed_address */, true);
    } break;

    case SYNTAX_IF_STATEMENT: {
      auto if_statement = syntax_assert_cast<Syntax_If_Statement>(node);
      auto cond_lc = typecheck(helper, if_statement->cond, parent_scope, params);
      require_to_be_a_value(cond_lc);

      if (!types_are_equal(helper->workspace->builtin_types.bool_, cond_lc->inferred_type) &&
          !cond_lc->inferred_type->is_list_type) {
        report_error(if_statement->cond->loc, "If statement condition expression");
        report_type_mismatch_message(helper->workspace->builtin_types.bool_, cond_lc->inferred_type);
        exit(1);
      }

      if (is_last_statement) {
        params.last_statement = true;
      }

      auto then_branch_lc
        = typecheck(helper, if_statement->then_branch, parent_scope, params);
      require_to_be_a_value(then_branch_lc);

      if (if_statement->else_branch) {
        auto else_branch_lc
          = typecheck(helper, if_statement->else_branch, parent_scope, params);
        require_to_be_a_value(else_branch_lc);

        bool one_of_types_is_void =
          types_are_equal(then_branch_lc->inferred_type, helper->workspace->builtin_types.void_)
          || types_are_equal(else_branch_lc->inferred_type, helper->workspace->builtin_types.void_);

        bool types_differ =
            !types_are_equal(then_branch_lc->inferred_type,
                             else_branch_lc->inferred_type);

        Type_Information *union_type = NULL;
        bool should_fill_union = false;
        bool should_merge_values = false;

        if (one_of_types_is_void) {
          result_lc->inferred_type = helper->workspace->builtin_types.void_;
          result_lc->value_kind = VALUE_KIND_VALUE;
        } else if (types_differ) {
          union_type = resolve_union_type(helper->workspace, then_branch_lc->inferred_type, else_branch_lc->inferred_type);
          result_lc->inferred_type = union_type;
          result_lc->value_kind = VALUE_KIND_VALUE;
          should_fill_union = true;
          result_lc->if_statement_should_fill_union = true;
        } else {
          result_lc->inferred_type = then_branch_lc->inferred_type;
          result_lc->value_kind = VALUE_KIND_VALUE;
          should_merge_values = true;
          result_lc->if_statement_should_merge_values = true;
        }

        if (should_fill_union) {
          set_value(result_lc, NULL/*union_address*/, true);
        }

        if (should_merge_values) {
          set_value(result_lc, NULL/*ir_result*/, !is_integral_type(result_lc->inferred_type));
        }

        result_lc->if_statement_else = else_branch_lc;
      } else {
        result_lc->inferred_type = helper->workspace->builtin_types.void_;
        result_lc->value_kind = VALUE_KIND_VALUE;
      }

      result_lc->if_statement_cond = cond_lc;
      result_lc->if_statement_then = then_branch_lc;

      result_lc->ir_generate_fn = generate_if_statement_ir;
    } break;

    case SYNTAX_PATTERN_MATCHING: {
      auto pattern_matching = syntax_assert_cast<Syntax_Pattern_Matching>(node);

      Type_Information *explicit_type = NULL;
      if (pattern_matching->left_explicit_type_expr) {
        auto explicit_type_lc
          = typecheck(helper, pattern_matching->left_explicit_type_expr, parent_scope, params);
        require_to_be_a_type(explicit_type_lc);

        explicit_type = explicit_type_lc->inferred_type;
      }

      auto params_copy = params;
      params_copy.wanted_type = explicit_type; // NULL is also ok

      auto rhs_lc = typecheck(helper, pattern_matching->right, parent_scope, params_copy);
      require_to_be_a_value(rhs_lc);

      if (explicit_type) {
        if (!types_are_equal(explicit_type, rhs_lc->inferred_type)) {
          report_error(pattern_matching->left_explicit_type_expr->loc, "Explicit type does not match type of the value");
          report_error(pattern_matching->right->loc, "Value");
          report_type_mismatch_message(explicit_type, rhs_lc->inferred_type);
          exit(1);
        }
      }

      result_lc->block_statements.append(rhs_lc);
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        FOR (lc->block_statements) {
          AST *statement = *it;
          generate_ir(w, statement);
        }
        lc->ir_value_ = lc->block_statements[0]->ir_value_;
      };

      copy_value(result_lc, rhs_lc);

      traverse_pattern_matching(helper, parent_scope, pattern_matching->left, rhs_lc, &result_lc->block_statements);
    } break;

    case SYNTAX_CAST: {
      auto cast = syntax_assert_cast<Syntax_Cast>(node);

      auto to_type_lc = typecheck(helper, cast->to_type_expr, parent_scope, params);
      require_to_be_a_type(to_type_lc);

      result_lc->inferred_type = to_type_lc->inferred_type;

      auto params_copy = params;
      params_copy.wanted_type = to_type_lc->inferred_type;

      auto value_lc = typecheck(helper, cast->value, parent_scope, params_copy);
      require_to_be_a_value(value_lc);

      if (types_are_equal(to_type_lc->inferred_type, value_lc->inferred_type)) {
        result_lc = value_lc;
        goto done;
      }

      auto is_int = [&] (auto l) { return types_are_equal(l->inferred_type, helper->workspace->builtin_types.i64); };
      auto is_float = [&] (auto l) { return types_are_equal(l->inferred_type, helper->workspace->builtin_types.f64); };
      auto is_bool = [&] (auto l) { return types_are_equal(l->inferred_type, helper->workspace->builtin_types.bool_); };

      bool is_itof = is_int(value_lc) && is_float(to_type_lc);
      bool is_ftoi = is_float(value_lc) && is_int(to_type_lc);
      bool is_btoi = is_bool(value_lc) && is_int(to_type_lc);
      bool is_itob = is_int(value_lc) && is_bool(to_type_lc);

      if (is_itof || is_ftoi || is_btoi || is_itob) {
        // It is ok
      } else {
        report_error(cast->loc, "Cast from '%s' to '%s' is not allowed",
                     type_to_string(value_lc->inferred_type),
                     type_to_string(to_type_lc->inferred_type)
                     );
        exit(1);
      }

      result_lc->cast_value = value_lc;
      IR_Value *ir_result = NULL;
      if (is_ftoi) {
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          generate_ir(w, lc->cast_value);
          IR_Value *rvalue = get_rvalue(w, lc->cast_value);
          lc->ir_value_ = w->create_cast_float_to_integer(rvalue);
        };
      } else if (is_itof) {
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          generate_ir(w, lc->cast_value);
          IR_Value *rvalue = get_rvalue(w, lc->cast_value);
          lc->ir_value_ = w->create_cast_integer_to_float(rvalue);
        };
      } else if (is_btoi) {
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          generate_ir(w, lc->cast_value);
          IR_Value *rvalue = get_rvalue(w, lc->cast_value);
          lc->ir_value_ = rvalue;
        };
      } else if (is_itob) {
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          generate_ir(w, lc->cast_value);
          IR_Value *rvalue = get_rvalue(w, lc->cast_value);
          lc->ir_value_ = w->create_signed_int_cmp_not_equal(rvalue, ir_create_immediate_i64(0));
        };
      }
      set_value(result_lc, ir_result, false);
    } break;

    case SYNTAX_TRUE:
    case SYNTAX_FALSE:
      result_lc->inferred_type = helper->workspace->builtin_types.bool_;
      set_value(result_lc, NULL/*ir_create_immediate_i64(node->type == AST_TRUE)*/, false);
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        lc->ir_value_ = ir_create_immediate_i64(lc->syntax->type == SYNTAX_TRUE);
      };
      break;

    case SYNTAX_TUPLE_LITERAL: {
      auto tuple = syntax_assert_cast<Syntax_Tuple_Literal>(node);

      if (tuple->items.len == 0) {
        report_error(tuple->loc, "Empty tuples are not allowed");
        exit(1);
      }

      Array<AST *> typechecked_items = {};

      Array<Type_Information *> types = {};
      bool have_types = false;
      bool have_values = false;
      FOR (tuple->items) {
        Syntax_Tuple_Literal_Item *item = *it;

        Type_Information *optional_explicit_type = NULL;

        if (item->optional_type_expr) {
          auto optional_type_lc = typecheck(helper, item->optional_type_expr, parent_scope, params);
          require_to_be_a_type(optional_type_lc);
          optional_explicit_type = optional_type_lc->inferred_type;
        }

        auto params_copy = params;
        params_copy.wanted_type = optional_explicit_type;

        auto item_lc = typecheck(helper, item->value, parent_scope, params_copy);

        if (optional_explicit_type && !types_are_equal(optional_explicit_type, item_lc->inferred_type)) {
          report_error(item->value->loc, "Explicit type does not match value type");
          report_type_mismatch_message(optional_explicit_type, item_lc->inferred_type);
          exit(1);
        }

        if (is_a_type(item_lc)) {
          have_types = true;
        } else if (is_a_value(item_lc)) {
          have_values = true;
        } else {
          report_error(item->loc, "Tuple items should either type or value");
          exit(1);
        }

        types.append(item_lc->inferred_type);
        typechecked_items.append(item_lc);
      }

      if (have_types && have_values) {
        report_error(tuple->loc, "Tuple is a mix of values and types");
        exit(1);
      } else if (have_types) {
        result_lc->value_kind = VALUE_KIND_TYPE;
      } else if (have_values) {
        result_lc->value_kind = VALUE_KIND_VALUE;
      } else {
        assert(0 && "should not be hit");
      }

      result_lc->inferred_type = resolve_tuple_type(helper->workspace, types, tuple->loc);

      if (is_a_value(result_lc)) {
        result_lc->tuple_literal_items = typechecked_items;
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          auto tuple_type = lc->inferred_type;
          IR_Value *address = ir_dynamic_memory_allocation(w, ir_create_immediate_i64(tuple_type->size));

          assert(tuple_type->tuple_items.len == lc->tuple_literal_items.len);
          for (int i = 0; i < tuple_type->tuple_items.len; i++) {
            auto tuple_item = tuple_type->tuple_items[i];
            auto typechecked_item = lc->tuple_literal_items[i];
            generate_ir(w, typechecked_item);

            IR_Value *address_after_offset = w->create_int_add(
              address, ir_create_immediate_i64(tuple_item.offset));

            auto size = tuple_item.type->size;
            if (is_integral_type(tuple_item.type)) {
              assert(size == 8); //for now only support 64 bit fields in struct and struct literals
              auto rvalue = get_rvalue(w, typechecked_item);
              w->create_64bit_store(address_after_offset, rvalue);
            } else {
              auto lvalue = get_lvalue(w, typechecked_item);
              IR_Value *size_const = ir_create_immediate_i64(size);
              w->create_copy_bytes(address_after_offset, lvalue, size_const);
            }
          }
          lc->ir_value_ = address;
        };

        set_value(result_lc, NULL/*address*/, true);
      }
    } break;

    case SYNTAX_SUBSCRIPT: {
      auto subscript = syntax_assert_cast<Syntax_Subscript>(node);

      auto array_like_lc = typecheck(helper, subscript->array, parent_scope, params);

      auto index_lc = typecheck(helper, subscript->index, parent_scope, params);

      require_to_be_a_value(array_like_lc);
      require_to_be_a_value(index_lc);

      if (array_like_lc->inferred_type->is_tuple_type) {
        if (!types_are_equal(helper->workspace->builtin_types.i64, index_lc->inferred_type)) {
          report_type_mismatch_message(helper->workspace->builtin_types.i64, index_lc->inferred_type);
          report_error(subscript->index->loc, "Here");
          exit(1);
        }

        if (subscript->index->type != SYNTAX_NUMBER_LITERAL) {
          report_error(subscript->index->loc, "At the moment only number literals are allowed");
          exit(1);
        }
        auto index_as_number_literal = syntax_assert_cast<Syntax_Number_Literal>(subscript->index);

        auto field_no = atoi(index_as_number_literal->value);
        auto tuple_type = array_like_lc->inferred_type;
        if (field_no < 0 || field_no >= tuple_type->tuple_items.len) {
          report_error(subscript->index->loc, "Out of bound [0, %d)", tuple_type->tuple_items.len);
          exit(1);
        }
        auto field = tuple_type->tuple_items[field_no];

        result_lc->subscript_array_like = array_like_lc;
        result_lc->subscript_index = field_no;
        result_lc->ir_generate_fn = generate_tuple_subscript_ir;

        set_value(result_lc, NULL/*address_after_offset*/, true);
        result_lc->inferred_type = field.type;
      } else {
        report_error(subscript->array->loc, "Subscript is not allowed");
        exit(1);
      }
    } break;

    case SYNTAX_CASE_STATEMENT: {
      auto case_statement = syntax_assert_cast<Syntax_Case_Statement>(node);

      auto value_lc = typecheck(helper, case_statement->value, parent_scope, params);
      require_to_be_a_value(value_lc);

      auto union_type = value_lc->inferred_type;
      assert(union_type->is_union_type);

      // IR_Value *union_tag_value = ir_writer.create_64bit_load(get_lvalue(value_lc));
      // IR_Value *incoming_data_address = ir_writer.create_int_add(get_lvalue(value_lc), ir_create_immediate_i64(helper->workspace->builtin_types.i64->size));
      //
      // IR_Block *after_block = helper->ir_scratchpad->create_block();

      result_lc->case_statement_cond = value_lc;

      FOR (case_statement->branches) {
        Syntax_Case_Statement_Branch *branch = *it;

        Syntax *type_to_be_expr = NULL;
        const char *variable_name = NULL;
        if (branch->type_expr) {
          assert(branch->value);
          if (branch->value->type != SYNTAX_IDENTIFIER) {
            report_error(branch->value->loc, "Expected to be an identifier");
            exit(1);
          }
          variable_name = syntax_assert_cast<Syntax_Identifier>(branch->value)->name;
          type_to_be_expr = branch->type_expr;
        } else {
          if (case_statement->value->type != SYNTAX_IDENTIFIER) {
            report_error(case_statement->value->loc, "Expected to be an identifier");
            report_error(branch->loc, "Because branch did not provide type and variable name at the same time");
            exit(1);
          }
          variable_name = syntax_assert_cast<Syntax_Identifier>(case_statement->value)->name;
          type_to_be_expr = branch->value;
        }
        assert(variable_name);
        assert(type_to_be_expr);

        auto branch_type_lc = typecheck(helper, type_to_be_expr, parent_scope, params);
        require_to_be_a_type(branch_type_lc);

        int found = -1;
        for (int i = 0; i < union_type->union_type_variants.len; ++i) {
          if (types_are_equal(union_type->union_type_variants[i], branch_type_lc->inferred_type)) {
            found = i;
            break;
          }
        }

        if (found == -1) {
          report_error(branch->loc, "Type '%s' not found in union type of value", type_to_string(branch_type_lc->inferred_type));
          exit(1);
        }

        AST *variable_lc = alloc<AST>();
        variable_lc->is_type_checking_done = true;
        variable_lc->inferred_type = branch_type_lc->inferred_type;
        set_value(variable_lc, NULL/*incoming_data_address*/, true);

        Scope *branch_scope = alloc<Scope>();
        branch_scope->parent_scope = parent_scope;
        branch_scope->add(variable_name, variable_lc);

        auto branch_body_lc = typecheck(helper, branch->body, branch_scope, params);

        result_lc->case_statement_branches.append({
          .type_index = found,
          .variable_lc = variable_lc,
          .branch_lc = branch_body_lc
        });
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
          generate_ir(w, lc->case_statement_cond);

          IR_Value *union_address = get_lvalue(w, lc->case_statement_cond);
          IR_Value *union_tag_value = w->create_64bit_load(union_address);
          IR_Value *data_address = w->create_int_add(union_address, ir_create_immediate_i64(w->workspace->builtin_types.i64->size));

          IR_Block *after_block = w->scratchpad->create_block();

          FOR (lc->case_statement_branches) {
            Case_Statement_Branch *branch = it;
            branch->variable_lc->ir_value_ = data_address;

            IR_Block *branch_block = w->scratchpad->create_block();
            IR_Block *else_block = w->scratchpad->create_block();
            IR_Value *branch_cond = w->create_signed_int_cmp_equal(union_tag_value, ir_create_immediate_i64(branch->type_index + 1));
            w->create_cond_jump(branch_cond, branch_block, else_block);


            w->last_instruction = &branch_block->start_stub;
            generate_ir(w, branch->branch_lc);
            w->create_jump(after_block);

            w->last_instruction = &else_block->start_stub;
          }

          w->create_jump(after_block);
          w->last_instruction = &after_block->start_stub;
        };
      }

      result_lc->inferred_type = helper->workspace->builtin_types.void_;
      result_lc->value_kind = VALUE_KIND_VALUE;
    } break;

    case SYNTAX_ATOM: {
      auto atom = syntax_assert_cast<Syntax_Atom>(node);
      Type_Information *atom_type = resolve_atom_type(helper->workspace, atom->name);
      result_lc->inferred_type = atom_type;
      result_lc->value_kind = VALUE_KIND_ATOM;
      result_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
        assert(lc->inferred_type->is_atom_type);
        lc->ir_value_ = ir_create_immediate_i64(lc->inferred_type->atom_index);
      };
    } break;

    case SYNTAX_FUNCTION_TYPE: {
      auto function_type_expr = syntax_assert_cast<Syntax_Function_Type>(node);

      Array<Type_Information *> arg_types = {};
      Type_Information *return_type = NULL;

      FOR (function_type_expr->arg_type_exprs) {
        Syntax *arg_type_expr = *it;

        auto arg_type_expr_lc = typecheck(helper, arg_type_expr, parent_scope, params);
        require_to_be_a_type(arg_type_expr_lc);

        arg_types.append(arg_type_expr_lc->inferred_type);
      }

      auto return_type_expr_lc = typecheck(helper, function_type_expr->return_type_expr, parent_scope, params);
      require_to_be_a_type(return_type_expr_lc);

      return_type = return_type_expr_lc->inferred_type;

      result_lc->inferred_type = resolve_function_type(helper->workspace, arg_types, return_type);
      result_lc->value_kind = VALUE_KIND_TYPE;
    } break;

    case SYNTAX_LIST_LITERAL: {
      auto list_literal = syntax_assert_cast<Syntax_List_Literal>(node);
      Type_Information *item_type = NULL;

      if (list_literal->explicit_item_type) {
        AST *explicit_item_type_lc = typecheck(helper, list_literal->explicit_item_type, parent_scope, params);
        require_to_be_a_type(explicit_item_type_lc);
        item_type = explicit_item_type_lc->inferred_type;
      } else {
        if (list_literal->items.len == 0) {
          report_error(list_literal->loc, "Expected at least 1 item in list literal to infer it's types");
          exit(1);
        }
      }

      bool have_types = false;
      bool have_values = false;
      Array<AST *> item_lcs = {};
      AST *rest = NULL;

      if (list_literal->rest) {
        rest = typecheck(helper, list_literal->rest, parent_scope, params);
        require_to_be_a_value(rest);
      }

      FOR (list_literal->items) {
        Syntax *item = *it;
        auto item_lc = typecheck(helper, item, parent_scope, params);

        if (is_a_type(item_lc)) have_types = true;
        if (is_a_value(item_lc)) have_values = true;

        if (have_types && have_values) {
          report_error(item->loc, "Mix between values and types in list literal");
          exit(1);
        }

        //require_to_be_a_value(item_lc);
        if (!item_type) {
          item_type = item_lc->inferred_type;
        } else if (!types_are_equal(item_type, item_lc->inferred_type)){
          report_error(item->loc, "List item type does not match type of list");
          report_type_mismatch_message(item_type, item_lc->inferred_type);
          exit(1);
        }
        item_lcs.append(item_lc);
      }


      result_lc->inferred_type = resolve_list_type(helper->workspace, item_type);
      if (have_types) {
        if (item_lcs.len > 1) {
          report_error(list_literal->loc, "Unnecessary item list type literal");
          exit(1);
        }
        if (rest) {
          report_error(list_literal->loc, "Unnecessary rest value in list type literal");
          exit(1);
        }
        result_lc->value_kind = VALUE_KIND_TYPE;
      } else {
        if (rest) {
          if (!types_are_equal(result_lc->inferred_type, rest->inferred_type)) {
            report_error(list_literal->rest->loc, "Type of the rest in list literal does not match type of list");
            report_type_mismatch_message(result_lc->inferred_type, rest->inferred_type);
            exit(1);
          }
        }
        result_lc->value_kind = VALUE_KIND_VALUE;
        result_lc->is_lvalue_ = false;
        result_lc->list_literal_rest = rest;
        result_lc->list_literal_items = item_lcs;
        result_lc->ir_generate_fn = [] (IR_Writer *w, AST *list_literal) {
          FOR (list_literal->list_literal_items) {
            generate_ir(w, *it);
          }
          if (list_literal->list_literal_rest) {
            generate_ir(w, list_literal->list_literal_rest);
          }

          IR_Value *next = NULL;

          if (list_literal->list_literal_items.len == 0) {
            next = ir_create_immediate_i64(0);
          } else for (int i = list_literal->list_literal_items.len - 1; i >= 0; i--) {
            AST *item = list_literal->list_literal_items[i];

            auto forward_list_node = ir_dynamic_memory_allocation(
              w,
              ir_create_immediate_i64(list_literal->inferred_type->size));

            if (next) {
              w->create_64bit_store(forward_list_node, next);
            } else {
              if (list_literal->list_literal_rest) {
                w->create_64bit_store(forward_list_node, list_literal->list_literal_rest->ir_value_);
              } else {
                w->create_64bit_store(forward_list_node, ir_create_immediate_i64(0));
              }
            }

            auto data_ir = w->create_int_add(
              forward_list_node,
              ir_create_immediate_i64(w->workspace->builtin_types.rawptr->size));

            assert(list_literal->inferred_type->is_list_type);
            if (list_literal->inferred_type->list_item_type->is_primitive_type) {
              auto rvalue = get_rvalue(w, item);
              w->create_64bit_store(data_ir, rvalue);
            } else {
              auto lvalue = get_lvalue(w, item);
              w->create_64bit_store(data_ir, lvalue);
            }

            next = forward_list_node;
          }

          assert(next);
          list_literal->ir_value_ = next;
        };
      }
    } break;

    default: {
      fprintf(stderr, "%s:%d:%s unhandled %s\n", __FILE__, __LINE__, __func__, to_string(node->type));
      assert(0 && "unhandled case");
    }
  }
done:

  result_lc->is_type_checking_done = true;

  if (wanted_type) {
    if (is_a_value(result_lc) && wanted_type->is_union_type) {
      AST *promoted = {};
      if (emit_promotion_to_union(result_lc, wanted_type, &promoted)) {
        promoted->inferred_type = wanted_type;
        promoted->is_type_checking_done = true;
        result_lc = promoted;
      }
    }
  }

  if (params.enclosing_function) {
    result_lc->belongs_to_function = params.enclosing_function;
  }

  if (node->type == SYNTAX_FUNCTION && params.enclosing_function) {
    AST *fat_pointer_lc = alloc<AST>();
    *fat_pointer_lc = *result_lc;
    fat_pointer_lc->promotion_to_function_pointer_function = result_lc;
    fat_pointer_lc->promotion_to_function_pointer_parent_scope = parent_scope;

    fat_pointer_lc->ir_generate_fn = [] (IR_Writer *w, AST *lc) {
      AST *fn = lc->promotion_to_function_pointer_function;
      Scope *parent_scope = lc->promotion_to_function_pointer_parent_scope;

      IR_Value *fat_function_pointer = ir_dynamic_memory_allocation(
        w, ir_create_immediate_i64(fn->inferred_type->size));

      w->create_64bit_store(fat_function_pointer, fn->ir_function);

      IR_Value *closure_pointer_inside_fat_function_pointer =
        w->create_int_add(
          fat_function_pointer,
          ir_create_immediate_i64(w->workspace->builtin_types.rawptr->size));


      if (fn->closure_capture_struct) {
        IR_Value *closure_base = ir_dynamic_memory_allocation(
          w,
          ir_create_immediate_i64(fn->closure_capture_struct->size));

        FOR (fn->closure_capture_struct->struct_fields) {
          Struct_Field field = *it;
          AST *external_value_lc = parent_scope->find(field.name);
          assert(is_a_value(external_value_lc));

          IR_Value *dest_address = w->create_int_add(
            closure_base,
            ir_create_immediate_i64(field.offset));

          copy_ir_value(w, dest_address, external_value_lc);
        }

        w->create_64bit_store(
          closure_pointer_inside_fat_function_pointer,
          closure_base);
      } else {
        w->create_64bit_store(
          closure_pointer_inside_fat_function_pointer,
          ir_create_immediate_i64(0));
      }
      lc->ir_value_ = fat_function_pointer;
    };
    set_value(fat_pointer_lc, NULL/*fat_function_pointer*/, true);

    result_lc = fat_pointer_lc;
  }

  assert(result_lc->inferred_type);
  return result_lc;
}
