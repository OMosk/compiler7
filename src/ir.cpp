#include "compiler.hpp"

#include <stdio.h>

const char *to_string(IR_Value_Type type) {
  switch (type) {
    case IR_VALUE_CONSTANT_GLOBAL: return "CONSTANT_GLOBAL";
    case IR_VALUE_CONSTANT_STRING_STRUCT: return "CONSTANT_STRING_STRUCT";
    case IR_VALUE_IMMEDIATE: return "IMMEDIATE";
    case IR_VALUE_REGISTER: return "REGISTER";
    case IR_VALUE_FUNCTION: return "FUNCTION";
    case IR_VALUE_ARGUMENT: return "ARGUMENT";
    case IR_VALUE_STATIC_ALLOCA: return "ALLOCA";
    case IR_VALUE_CLOSURE_BASE: return "CLOSURE_BASE";

    case IR_VALUE_NOOP:
    case IR_VALUE_COUNT:
      assert(0 && "should not use this values");
      break;
  }
}

const char *to_string(IR_Instruction_Type type) {
  switch (type) {
    case IR_INSTRUCTION_RET:  return "RET";
    case IR_INSTRUCTION_SIGNED_INT_ADD:  return "INT_ADD";
    case IR_INSTRUCTION_SIGNED_INT_SUB:  return "INT_SUB";
    case IR_INSTRUCTION_SIGNED_INT_MUL:  return "INT_MUL";
    case IR_INSTRUCTION_SIGNED_INT_DIV:  return "INT_DIV";
    case IR_INSTRUCTION_SIGNED_INT_REMAINDER:  return "INT_REMAINDER";
    case IR_INSTRUCTION_SIGNED_INT_BITWISE_OR:  return "INT_BITWISE_OR";
    case IR_INSTRUCTION_SIGNED_INT_BITWISE_AND:  return "INT_BITWISE_AND";
    case IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR:  return "INT_BITWISE_XOR";

    case IR_INSTRUCTION_SIGNED_INT_LOGICAL_OR:   return "INT_LOGICAL_OR";
    case IR_INSTRUCTION_SIGNED_INT_LOGICAL_AND:  return "INT_LOGICAL_AND";
    case IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT:  return "INT_LOGICAL_NOT";

    case IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL:            return "SIGNED_INT_CMP_EQUAL";
    case IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL:        return "SIGNED_INT_CMP_NOT_EQUAL";
    case IR_INSTRUCTION_SIGNED_INT_CMP_LESS:             return "SIGNED_INT_CMP_LESS";
    case IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL:    return "SIGNED_INT_CMP_LESS_OR_EQUAL";
    case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER:          return "SIGNED_INT_CMP_GREATER";
    case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL: return "SIGNED_INT_CMP_GREATER_OR_EQUAL";

    case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT:  return "SIGNED_INT_BITWISE_SHIFT_LEFT";
    case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT: return "SIGNED_INT_BITWISE_SHIFT_RIGHT";

    case IR_INSTRUCTION_FLOAT_ADD:  return "FLOAT_ADD";
    case IR_INSTRUCTION_FLOAT_SUB:  return "FLOAT_SUB";
    case IR_INSTRUCTION_FLOAT_MUL:  return "FLOAT_MUL";
    case IR_INSTRUCTION_FLOAT_DIV:  return "FLOAT_DIV";

    case IR_INSTRUCTION_FLOAT_CMP_EQUAL:            return "FLOAT_CMP_EQUAL";
    case IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL:        return "FLOAT_CMP_NOT_EQUAL";
    case IR_INSTRUCTION_FLOAT_CMP_LESS:             return "FLOAT_CMP_LESS";
    case IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL:    return "FLOAT_CMP_LESS_OR_EQUAL";
    case IR_INSTRUCTION_FLOAT_CMP_GREATER:          return "FLOAT_CMP_GREATER";
    case IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL: return "FLOAT_CMP_GREATER_OR_EQUAL";

    case IR_INSTRUCTION_CAST_FLOAT_TO_INTEGER_USING_TRUNCATION: return "CAST_FLOAT_TO_INTEGER_USING_TRUNCATION";
    case IR_INSTRUCTION_CAST_INTEGER_TO_FLOAT: return "CAST_INTEGER_TO_FLOAT";

    case IR_INSTRUCTION_CALL:  return "CALL";
    case IR_INSTRUCTION_TAIL_CALL:  return "TAIL_CALL";
    case IR_INSTRUCTION_SYSCALL:  return "SYSCALL";
    case IR_INSTRUCTION_FILL_WITH_ZEROES:  return "FILL_ZEROES";
    case IR_INSTRUCTION_COPY_BYTES:  return "COPY_BYTES";
    case IR_INSTRUCTION_64BIT_LOAD:  return "64BIT_LOAD";
    case IR_INSTRUCTION_64BIT_STORE:  return "64BIT_STORE";
    case IR_INSTRUCTION_COND_JUMP:  return "COND_JUMP";
    case IR_INSTRUCTION_JUMP:  return "JUMP";
    case IR_INSTRUCTION_VALUE_MERGE:  return "VALUE_MERGE";

    case IR_INSTRUCTION_NOOP:
    case IR_INSTRUCTION_COUNT:
      assert(0 && "should not use this values");
      break;
  }
}

IR_Instruction *IR_Writer::create_instruction(IR_Instruction_Type type) {
  auto instruction = alloc<IR_Instruction>();
  instruction->type = type;
  instruction->loc = loc;
  //instructions.append(instruction);

  if (last_instruction == NULL) {
    last_instruction = instruction;
  } else {
    last_instruction->next = instruction;
    last_instruction = last_instruction->next;
  }

  return instruction;
}

void IR_Writer::create_ret(IR_Value *result) {
  auto instruction = create_instruction(IR_INSTRUCTION_RET);
  instruction->op1 = result;
}

IR_Value *IR_Writer::create_two_operand_instruction(
  IR_Instruction_Type type, IR_Value *left, IR_Value *right) {

  auto instruction = create_instruction(type);
  instruction->op1 = left;
  instruction->op2 = right;
  instruction->res = scratchpad->create_temporary();

  return instruction->res;
}

IR_Value *IR_Writer::create_one_operand_instruction(IR_Instruction_Type type, IR_Value *left) {
  assert(left);
  return create_two_operand_instruction(type, left, NULL);
}

IR_Value *IR_Writer::create_int_add(IR_Value *left, IR_Value *right) {
  return create_two_operand_instruction(IR_INSTRUCTION_SIGNED_INT_ADD, left, right);
}

IR_Value *IR_Writer::create_int_mul(IR_Value *left, IR_Value *right) {
  return create_two_operand_instruction(IR_INSTRUCTION_SIGNED_INT_MUL, left, right);
}
IR_Value *IR_Writer::create_signed_int_cmp_equal(IR_Value *left, IR_Value *right) {
  return create_two_operand_instruction(IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL, left, right);
}

IR_Value *IR_Writer::create_signed_int_cmp_not_equal(IR_Value *left, IR_Value *right) {
  return create_two_operand_instruction(IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL, left, right);
}

IR_Value *IR_Scratchpad::create_temporary() {
  auto value = alloc<IR_Value>();
  value->type = IR_VALUE_REGISTER;
  //value->register_number = temporaries_count;
  value->name = alloc_sprintf("%%%p", value);
  temporaries.append(value);

  return value;
}

IR_Block *IR_Scratchpad::create_block() {
  auto block = alloc<IR_Block>();
  blocks.append(block);
  block->name = alloc_sprintf("block_%p", block);
  return block;
}

void IR_Scratchpad::incorporate(IR_Scratchpad *other) {
  FOR (other->blocks) {
    blocks.append(*it);
  }
  FOR (other->temporaries) {
    temporaries.append(*it);
  }
}

IR_Value *IR_Function::create_argument() {
  auto value = alloc<IR_Value>();
  value->type = IR_VALUE_ARGUMENT;
  value->argument_number = args.len;
  value->name = alloc_sprintf("arg_%d", value->argument_number);

  args.append(value);
  return value;
}

IR_Value *IR_Writer::create_cast_float_to_integer(IR_Value *left) {
  return create_two_operand_instruction(IR_INSTRUCTION_CAST_FLOAT_TO_INTEGER_USING_TRUNCATION, left, NULL);
}

IR_Value *IR_Writer::create_cast_integer_to_float(IR_Value *left) {
  return create_two_operand_instruction(IR_INSTRUCTION_CAST_INTEGER_TO_FLOAT, left, NULL);
}

IR_Value *IR_Writer::create_call(IR_Value *callee, Array<IR_Value *> args, IR_Value *closure) {
  assert(callee);
  auto i = create_instruction(IR_INSTRUCTION_CALL);
  i->res = scratchpad->create_temporary();
  i->op1 = callee;
  i->call_args = args;
  i->closure = closure;

  return i->res;
}

IR_Value *IR_Writer::create_tail_call(IR_Value *callee, Array<IR_Value *> args, IR_Value *closure) {
  assert(callee);
  auto i = create_instruction(IR_INSTRUCTION_TAIL_CALL);
  i->res = scratchpad->create_temporary();
  i->op1 = callee;
  i->call_args = args;
  i->closure = closure;

  return i->res;
}

IR_Value *IR_Writer::create_syscall(Array<IR_Value *> args) {
  assert(args.len);
  auto i = create_instruction(IR_INSTRUCTION_SYSCALL);
  i->res = scratchpad->create_temporary();
  i->call_args = args;

  return i->res;
}

void IR_Writer::create_fill_with_zeroes(IR_Value *address, IR_Value *size) {
  assert(address);
  assert(size);
  auto i = create_instruction(IR_INSTRUCTION_FILL_WITH_ZEROES);
  i->op1 = address;
  i->op2 = size;
}

void IR_Writer::create_copy_bytes(IR_Value *to, IR_Value *from, IR_Value *size) {
  assert(to);
  assert(from);
  assert(size);
  auto i = create_instruction(IR_INSTRUCTION_COPY_BYTES);
  i->op1 = to;
  i->op2 = from;
  i->op3 = size;
}

IR_Value *IR_Writer::create_64bit_load(IR_Value *from) {
  assert(from);
  auto i = create_instruction(IR_INSTRUCTION_64BIT_LOAD);
  i->op1 = from;
  i->res = scratchpad->create_temporary();
  return i->res;
}

void IR_Writer::create_64bit_store(IR_Value *to, IR_Value *v) {
  assert(to);
  assert(v);
  auto i = create_instruction(IR_INSTRUCTION_64BIT_STORE);
  i->op1 = to;
  i->op2 = v;
}

void IR_Writer::create_cond_jump(IR_Value *cond, IR_Block *then_block, IR_Block *else_block) {
  assert(cond);
  assert(then_block);
  assert(else_block);

  auto i = create_instruction(IR_INSTRUCTION_COND_JUMP);
  i->op1 = cond;
  i->then_block = then_block;
  i->else_block = else_block;
}

void IR_Writer::create_jump(IR_Block *block) {
  assert(block);
  auto i = create_instruction(IR_INSTRUCTION_JUMP);
  i->then_block = block;
}

IR_Value *IR_Writer::create_value_merge(IR_Value *cond, IR_Value *then_value, IR_Value *else_value) {
  assert(cond);
  assert(then_value);
  assert(else_value);

  auto i = create_instruction(IR_INSTRUCTION_VALUE_MERGE);
  i->op1 = cond;
  i->op2 = then_value;
  i->op3 = else_value;
  i->res = scratchpad->create_temporary();
  return i->res;
}

static IR_Value closure_base = {
  .type = IR_VALUE_CLOSURE_BASE,
  .name = "closure_base",
};

IR_Value *ir_closure_base() {
  return &closure_base;
}

IR_Value *ir_create_immediate_i64(int64_t v) {
  IR_Value *result = alloc<IR_Value>();
  result->type = IR_VALUE_IMMEDIATE;
  result->imm_i64 = v;
  return result;
}

IR_Value *ir_create_immediate_f64(double v) {
  IR_Value *result = alloc<IR_Value>();
  result->type = IR_VALUE_IMMEDIATE;
  result->imm_f64 = v;
  return result;
}

IR_Value *ir_dynamic_memory_allocation(
  IR_Writer *writer,
  IR_Value *size) {

  Array<IR_Value *> args = {};
  IR_Value *callee = alloc<IR_Value>();
  callee->type = IR_VALUE_FUNCTION;
  assert(writer->workspace->support_library_functions.dynamic_allocation);
  callee->function = writer->workspace->support_library_functions.dynamic_allocation;
  callee->name = callee->function->name;
  args.append(size);
  auto result = writer->create_call(callee, args, NULL);
  return result;
}

IR_Function *IR_Hub::create_function(Syntax_Function *func) {
  auto ir_function = alloc<IR_Function>();
  ir_function->type = IR_VALUE_FUNCTION;
  if (func->support_library) {
    ir_function->name = func->name->name;
  } else {
    ir_function->name = alloc_sprintf("function_%d_%s", functions.len, func->name ? func->name->name : "");
  }
  ir_function->language_level_name = func->name ? func->name->name : "<anonymous function>";
  ir_function->builtin = func->builtin;
  ir_function->c_call = func->c_call;
  ir_function->support_library = func->support_library;
  ir_function->function = ir_function;

  functions.append(ir_function);

  return ir_function;
}

IR_Function *IR_Hub::create_support_library_function(const char *name) {
  auto ir_function = alloc<IR_Function>();
  ir_function->name = name;
  //ir_function->entry_block = ir_function->scratchpad.create_block(); // TODO: investigate why
  ir_function->c_call = true;
  ir_function->support_library = true;
  ir_function->function = ir_function;
  functions.append(ir_function);
  return ir_function;
}

void print_ir_value(FILE *out, IR_Value *value) {
  if (value->name) {
    fprintf(out,"%s", value->name);
  } else if (value->type == IR_VALUE_IMMEDIATE) {
    fprintf(out,"imm(%lu/%f)", (uint64_t)value->imm_i64, value->imm_f64);
  } else if (value->type == IR_VALUE_FUNCTION) {
    fprintf(out,"%s", value->function->name);
  } else {
    assert(0 && "unhandled case in print_ir_value");
  }
}

void IR_Hub::debug() {
  FILE *out = fopen("out.ir", "w");
  assert(out);

  fprintf(out,"IR_Hub debug start ---- \n");

  fprintf(out,"global constants start ---- \n");
  FOR (const_globals) {
    IR_Value *v = *it;
    fprintf(out,"  %s = '%.*s'\n", v->name, (int)v->len, v->data);
  }
  fprintf(out,"global constants end   ---- \n\n");

  fprintf(out,"global constants string structs start ---- \n");
  FOR (const_strings_structs) {
    IR_Value *v = *it;
    fprintf(out,"  %s = '%.*s'\n", v->name, (int)v->len, v->data);
  }
  fprintf(out,"global constants string structs  end   ---- \n\n");

  fprintf(out,"functions start ---- \n");
  FOR (functions) {
    IR_Function *ir_func = *it;
    fprintf(out,"  %s/%d %s = \n", ir_func->name, ir_func->args.len, ir_func->entry_point ? "(entry_point)" : "");

    FOR (ir_func->scratchpad.blocks) {
      IR_Block *block = *it;
      fprintf(out,"    %s:\n", block->name);
      int i = 0;
      for (IR_Instruction *it = block->start_stub.next; it; it = it->next) {
      //FOR (block->instructions) {
        //IR_Instruction *instr = *it;
        IR_Instruction *instr = it;
        fprintf(out,"\n");
        fprintf(out,"      [%4d] ", i);
        print_code_location(instr->loc, out);
        fprintf(out,"\n");
        fprintf(out,"      [%4d] ", i);
        fprintf(out,"%-15s ", to_string(instr->type));
        if (instr->res) {
          fprintf(out,"res:");
          print_ir_value(out, instr->res);
          fprintf(out," ");
        }
        if (instr->op1) {
          fprintf(out,"op1:");
          print_ir_value(out, instr->op1);
          fprintf(out," ");
        }
        if (instr->op2) {
          fprintf(out,"op2:");
          print_ir_value(out, instr->op2);
          fprintf(out," ");
        }
        if (instr->op3) {
          fprintf(out,"op3:");
          print_ir_value(out, instr->op3);
          fprintf(out," ");
        }
        if (instr->type == IR_INSTRUCTION_CALL ||
            instr->type == IR_INSTRUCTION_SYSCALL ||
            instr->type == IR_INSTRUCTION_TAIL_CALL) {
          fprintf(out,"args=(");
          bool first = true;
          FOR (instr->call_args) {
            if (!first) fprintf(out,", ");
            first = false;
            print_ir_value(out, *it);
          }
          fprintf(out,")");

          if (instr->closure) {
            fprintf(out," closure=");
            print_ir_value(out, instr->closure);
          }
        }

        if (instr->then_block) {
          fprintf(out,"%s ", instr->then_block->name);
        }

        if (instr->else_block) {
          fprintf(out,"%s ", instr->else_block->name);
        }

        fprintf(out,"\n");
        i++;
      }
      fprintf(out,"\n");
    }
  }
  fprintf(out,"functions end   ---- \n");

  fprintf(out,"IR_Hub debug end   ---- \n");
  fclose(out);
}

IR_Value *IR_Hub::create_const_data(const char *data, size_t len, const char *name) {
  auto v = alloc<IR_Value>();
  v->type = IR_VALUE_CONSTANT_GLOBAL;
  v->data = data;
  v->len = len;
  v->name = alloc_sprintf("const_global_%d_%s", const_globals.len, name ? name : "");
  
  const_globals.append(v);
  return v;
}

IR_Value *IR_Hub::create_const_string_struct(const char *data, size_t len, const char *name) {
  auto v = alloc<IR_Value>();
  v->type = IR_VALUE_CONSTANT_STRING_STRUCT;
  v->data = data;
  v->len = len;
  v->name = alloc_sprintf("const_string_%d_%s", const_strings_structs.len, name ? name : "");
  
  const_strings_structs.append(v);
  return v;
}
