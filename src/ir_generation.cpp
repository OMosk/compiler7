#include "compiler.hpp"

void generate_binary_op_ir_for_ints(IR_Writer *w, AST *lc) {
  auto binary_op = syntax_assert_cast<Syntax_Binary_Op>(lc->syntax);

  auto left_lc = lc->binary_op_left_operand_lc;
  auto right_lc = lc->binary_op_right_operand_lc;

  auto do_regular_handling = [&] (IR_Instruction_Type instruction_type) {
    generate_ir(w, left_lc);
    generate_ir(w, right_lc);

    IR_Value *left_rvalue  = get_rvalue(w, left_lc);
    IR_Value *right_rvalue = get_rvalue(w, right_lc);
    lc->ir_value_ = w->create_two_operand_instruction(instruction_type, left_rvalue, right_rvalue);
  };

  switch(binary_op->op) {
  case BINARY_OP_PLUS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_ADD); break;
  case BINARY_OP_MINUS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_SUB); break;
  case BINARY_OP_MULTIPLY: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_MUL); break;
  case BINARY_OP_DIVISION: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_DIV); break;
  case BINARY_OP_REMAINDER: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_REMAINDER); break;

  case BINARY_OP_BITWISE_OR: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_OR); break;
  case BINARY_OP_BITWISE_AND: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_AND); break;
  case BINARY_OP_BITWISE_XOR: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR); break;
  case BINARY_OP_BITWISE_SHIFT_LEFT: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT); break;
  case BINARY_OP_BITWISE_SHIFT_RIGHT: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT); break;
                          

  case BINARY_OP_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL); break;
  case BINARY_OP_NOT_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL); break;
  case BINARY_OP_LESS: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_LESS); break;
  case BINARY_OP_LESS_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL); break;
  case BINARY_OP_GREATER: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER); break;
  case BINARY_OP_GREATER_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL); break;

  default:
    assert(0 && "should not be reachable");
  }

  assert(lc->ir_value_);
}

void generate_binary_op_ir_for_floats(IR_Writer *w, AST *lc) {
  auto binary_op = syntax_assert_cast<Syntax_Binary_Op>(lc->syntax);

  auto left_lc = lc->binary_op_left_operand_lc;
  auto right_lc = lc->binary_op_right_operand_lc;

  auto do_regular_handling = [&] (IR_Instruction_Type instruction_type) {
    generate_ir(w, left_lc);
    generate_ir(w, right_lc);

    IR_Value *left_rvalue  = get_rvalue(w, left_lc);
    IR_Value *right_rvalue = get_rvalue(w, right_lc);
    lc->ir_value_ = w->create_two_operand_instruction(instruction_type, left_rvalue, right_rvalue);
  };

  switch(binary_op->op) {
  case BINARY_OP_PLUS: do_regular_handling(IR_INSTRUCTION_FLOAT_ADD); break;
  case BINARY_OP_MINUS: do_regular_handling(IR_INSTRUCTION_FLOAT_SUB); break;
  case BINARY_OP_MULTIPLY: do_regular_handling(IR_INSTRUCTION_FLOAT_MUL); break;
  case BINARY_OP_DIVISION: do_regular_handling(IR_INSTRUCTION_FLOAT_DIV); break;

  case BINARY_OP_LESS: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_LESS); break;
  case BINARY_OP_LESS_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL); break;
  case BINARY_OP_GREATER: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_GREATER); break;
  case BINARY_OP_GREATER_OR_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL); break;
  case BINARY_OP_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_EQUAL); break;
  case BINARY_OP_NOT_EQUAL: do_regular_handling(IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL); break;

  default:
    assert(0 && "should not be reachable");
  }

  assert(lc->ir_value_);
}

void generate_short_circuited_logical_operation(IR_Writer *w, AST *lc, int64_t early_exit_cond_value) {
  generate_ir(w, lc->binary_op_left_operand_lc);
  IR_Value *left_rvalue = get_rvalue(w, lc->binary_op_left_operand_lc);
  auto early_exit_cond = w->create_signed_int_cmp_equal(left_rvalue, ir_create_immediate_i64(early_exit_cond_value));
  IR_Instruction *before_block_last_instruction = w->last_instruction;

  auto rhs_computation_block = w->scratchpad->create_block();
  w->last_instruction = &rhs_computation_block->start_stub;
  generate_ir(w, lc->binary_op_right_operand_lc);
  IR_Value *right_rvalue = get_rvalue(w, lc->binary_op_right_operand_lc);
  IR_Value *rhs_result_boolean = w->create_signed_int_cmp_not_equal(right_rvalue, ir_create_immediate_i64(0));

  auto after_block = w->scratchpad->create_block();
  w->create_jump(after_block);

  w->last_instruction = before_block_last_instruction;
  w->create_cond_jump(early_exit_cond, after_block, rhs_computation_block);

  w->last_instruction = &after_block->start_stub;
  lc->ir_value_ = w->create_value_merge(early_exit_cond, ir_create_immediate_i64(early_exit_cond_value), rhs_result_boolean);
}

void generate_struct_literal_ir(IR_Writer *w, AST *lc) {
  if (lc->struct_literal_existing_value) {
    generate_ir(w, lc->struct_literal_existing_value);
  }

  FOR (lc->struct_literal_entries) {
    generate_ir(w, it->value);
  }

  auto struct_type = lc->inferred_type;
  IR_Value *address = ir_dynamic_memory_allocation(w, ir_create_immediate_i64(struct_type->size));

  if (lc->struct_literal_existing_value) {
    auto existing_lvalue = get_lvalue(w, lc->struct_literal_existing_value);
    w->create_copy_bytes(address, existing_lvalue, ir_create_immediate_i64(struct_type->size));
  } else {
    w->create_fill_with_zeroes(address, ir_create_immediate_i64(struct_type->size));
  }

  FOR (lc->struct_literal_entries) {
    Struct_Field struct_field = {};

    auto found = find_struct_field(struct_type, it->name, &struct_field);
    assert(found);

    IR_Value *field_address = w->create_int_add(address, ir_create_immediate_i64(struct_field.offset));

    if (is_integral_type(struct_field.type)) {
      assert(struct_field.type->size == 8); //for now only support 64 bit fields in struct and struct literals
      auto rvalue = get_rvalue(w, it->value);
      w->create_64bit_store(field_address, rvalue);
    } else {
      auto lvalue = get_lvalue(w, it->value);
      IR_Value *size_const = ir_create_immediate_i64(struct_field.type->size);
      w->create_copy_bytes(field_address, lvalue, size_const);
    }
  }
  
  lc->ir_value_ = address;
}

void generate_if_statement_ir(IR_Writer *w, AST *lc) {
  generate_ir(w, lc->if_statement_cond);
  auto cond_rvalue = get_rvalue(w, lc->if_statement_cond);

  if (lc->if_statement_else) {
    IR_Instruction *before_block_last_instruction = w->last_instruction;

    IR_Block *then_block = w->scratchpad->create_block();
    w->last_instruction = &then_block->start_stub;
    generate_ir(w, lc->if_statement_then);
    IR_Instruction *then_block_last_instruction = w->last_instruction;

    IR_Block *else_block = w->scratchpad->create_block();
    w->last_instruction = &else_block->start_stub;
    generate_ir(w, lc->if_statement_else);
    IR_Instruction *else_block_last_instruction = w->last_instruction;

    w->last_instruction = before_block_last_instruction;
    if (lc->if_statement_should_fill_union) {
      assert(lc->inferred_type->is_union_type);
      lc->ir_value_ = ir_dynamic_memory_allocation(w, ir_create_immediate_i64(lc->inferred_type->size));
    }
    w->create_cond_jump(cond_rvalue, then_block, else_block);

    IR_Block *after_block = w->scratchpad->create_block();

    {
      w->last_instruction = then_block_last_instruction;
      if (lc->if_statement_should_fill_union) {
        int index = 0;
        bool found = find_type_in_union(lc->inferred_type, lc->if_statement_then->inferred_type, &index);
        assert(found);
        copy_value_to_union(w, lc->ir_value_, index, lc->if_statement_then);
      }
      w->create_jump(after_block);
    }

    {
      w->last_instruction = else_block_last_instruction;
      if (lc->if_statement_should_fill_union) {
        int index = 0;
        bool found = find_type_in_union(lc->inferred_type, lc->if_statement_else->inferred_type, &index);
        assert(found);
        copy_value_to_union(w, lc->ir_value_, index, lc->if_statement_else);
      }
      w->create_jump(after_block);
    }

    w->last_instruction = &after_block->start_stub;
    if (lc->if_statement_should_merge_values) {
      lc->ir_value_ = w->create_value_merge(
        cond_rvalue, lc->if_statement_then->ir_value_, lc->if_statement_else->ir_value_);
    }
  } else {
    IR_Instruction *before_block_last_instruction = w->last_instruction;

    IR_Block *then_block = w->scratchpad->create_block();
    w->last_instruction = &then_block->start_stub;
    generate_ir(w, lc->if_statement_then);

    IR_Block *after_block = w->scratchpad->create_block();
    w->create_jump(after_block);

    w->last_instruction = before_block_last_instruction;
    w->create_cond_jump(cond_rvalue, then_block, after_block);

    w->last_instruction = &after_block->start_stub;
  }
}

void generate_struct_member_access_ir(IR_Writer *w, AST *lc) {
  Struct_Field field = {};
  bool found = find_struct_field(
    lc->member_access_struct->inferred_type, lc->member_access_field_name, &field);
  assert(found);

  generate_ir(w, lc->member_access_struct);
  IR_Value *struct_like_lvalue = get_lvalue(w, lc->member_access_struct);
  lc->ir_value_ = w->create_int_add(struct_like_lvalue, ir_create_immediate_i64(field.offset));
}

void generate_tuple_subscript_ir(IR_Writer *w, AST *lc) {
  generate_ir(w, lc->subscript_array_like);

  auto tuple_type = lc->subscript_array_like->inferred_type;
  assert(tuple_type->is_tuple_type);

  auto field = tuple_type->tuple_items[lc->subscript_index];
  auto address_of_array_like = get_lvalue(w, lc->subscript_array_like);

  lc->ir_value_ = w->create_int_add(address_of_array_like, ir_create_immediate_i64(field.offset));
}

void generate_string_comparison_ir(IR_Writer *w, AST *lc, IR_Instruction_Type cmp_op) {
  generate_ir(w, lc->binary_op_left_operand_lc);
  generate_ir(w, lc->binary_op_right_operand_lc);
  Array<IR_Value *> args = {};
  args.append(get_lvalue(w, lc->binary_op_left_operand_lc));
  args.append(get_lvalue(w, lc->binary_op_right_operand_lc));

  auto ir_comparison_result = w->create_call(
    w->workspace->support_library_functions.string_compare,
    args,
    NULL);

  lc->ir_value_ = w->create_two_operand_instruction(
    cmp_op,
    ir_comparison_result,
    ir_create_immediate_i64(0));

}

void generate_capture_ir(IR_Writer *w, AST *lc) {
  const char *name = lc->capture_name;
  Struct_Field field = {};
  bool found = find_struct_field(lc->capture_enclosing_function->closure_capture_struct, name, &field);
  assert(found);

  lc->ir_value_ = w->create_int_add(ir_closure_base(), ir_create_immediate_i64(field.offset));
}

void generate_function_ir(AST *workspace, AST *function_lc) {
  if (!function_lc->body_lc) return;

  function_lc->ir_function->entry_block = function_lc->ir_function->scratchpad.create_block();

  IR_Writer ir_writer = {};
  ir_writer.workspace = workspace;
  ir_writer.hub = &workspace->ir_hub;
  ir_writer.scratchpad = &function_lc->ir_function->scratchpad;
  ir_writer.last_instruction = &function_lc->ir_function->entry_block->start_stub;
  //TODO loc

  FOR (function_lc->captures) {
    AST *capture = *it;
    generate_ir(&ir_writer, capture);
  }

  generate_ir(&ir_writer, function_lc->body_lc);

  bool return_type_is_void = types_are_equal(
    function_lc->inferred_type->function_type_return, workspace->builtin_types.void_);

  if (return_type_is_void) {
    ir_writer.create_ret(NULL);
  } else {
    IR_Value *result = NULL;
    if (is_integral_type(function_lc->inferred_type->function_type_return)) {
      result = get_rvalue(&ir_writer, function_lc->body_lc);
    } else {
      result = get_lvalue(&ir_writer, function_lc->body_lc);
    }
    ir_writer.create_ret(result);
  }

  auto ir_function = function_lc->ir_function;

  ir_function->static_allocas_offset_from_bottom = 0;

  size_t temporaries_offset_from_rbp = 0;
  size_t arguments_size = ir_function->args.len * 8;

  {
    int index = 0;
    FOR (ir_function->scratchpad.temporaries) {
      auto temporary = *it;
      temporary->register_number = index++;
    }
  }

  ir_function->temporaries_offset_from_bottom = temporaries_offset_from_rbp;
  ir_function->preamble_allocation_size = 
    ir_function->temporaries_offset_from_bottom
    + ir_function->scratchpad.temporaries.len * 8
    + arguments_size
    + 8 /*closure base pointer pointer*/;
  ir_function->preamble_allocation_size = align(ir_function->preamble_allocation_size, 16);
}

void generate_ir(IR_Writer *ir_writer, AST *lc) {
  IR_Writer copy = *ir_writer;
  copy.loc = &lc->syntax->loc;

  assert(lc->ir_generate_fn);
  lc->ir_generate_fn(&copy, lc);

  ir_writer->last_instruction = copy.last_instruction;
}

