#include "compiler.hpp"

#include <stdlib.h>

// TODO: maybe it should be moved to ir.cpp since it is backend agnostic
int calculate_offset(IR_Function *f, IR_Value *v) {
  switch (v->type) {

  case IR_VALUE_REGISTER: {
    int offset =
      - f->preamble_allocation_size
      + f->temporaries_offset_from_bottom
      + 8*v->register_number;
    return offset;
  } break;

  case IR_VALUE_STATIC_ALLOCA: {
    int offset =
      - f->preamble_allocation_size
      + f->static_allocas_offset_from_bottom
      + v->alloca_offset;
    return offset;
  } break;

  case IR_VALUE_ARGUMENT: {
    int offset = - 8*(v->argument_number + 1) - 8/*to skip closure pointer*/;
    return offset;
  } break;

  default: assert(0 && "should not be used");
  }
}

void nasm_read_into_register(FILE *f, IR_Function *function, const char *register_name, IR_Value *v, int optional_offset) {
  switch (v->type) {
    case IR_VALUE_IMMEDIATE: {
      assert(optional_offset == 0);
      fprintf(f, "\tmov %s, %ld\n", register_name, v->imm_i64);
    } break;

    case IR_VALUE_CONSTANT_GLOBAL: {
      assert(optional_offset == 0);
      fprintf(f, "\tmov %s, %s\n", register_name, v->name);
    } break;

    case IR_VALUE_CONSTANT_STRING_STRUCT: {
      assert(optional_offset == 0);
      fprintf(f, "\tmov %s, %s\n", register_name, v->name);
    } break;

    case IR_VALUE_REGISTER: {
      int offset = calculate_offset(function, v);
      fprintf(f, "\tmov %s, [rbp + %d]\n", register_name, offset);
      if (optional_offset) {
        fprintf(f, "\tadd %s, %d\n", register_name, optional_offset);
      }
    } break;

    case IR_VALUE_ARGUMENT: {
      assert(optional_offset == 0);
      int offset = calculate_offset(function, v);
      fprintf(f, "\tmov %s, [rbp + %d]\n", register_name, offset);
    } break;
    case IR_VALUE_STATIC_ALLOCA: {
      int offset = calculate_offset(function, v) + optional_offset;
      fprintf(f, "\tlea %s, [rbp + %d]\n", register_name, offset);
    } break;

    case IR_VALUE_FUNCTION: {
      assert(optional_offset == 0);
      fprintf(f, "\tmov %s, %s\n", register_name, v->name);
    } break;

    case IR_VALUE_CLOSURE_BASE: {
      fprintf(f, "\tmov %s, [rbp - 8]\n", register_name);
    } break;

    case IR_VALUE_NOOP:
    case IR_VALUE_COUNT: {
      assert(0 && "This value should not be encountered");
    } break;

    default: {
      printf("%s\n", to_string(v->type));
      assert(0 && "not implemented");
    }
  }
}
void nasm_write_from_register (FILE *out, IR_Function *f, IR_Value *v, const char *register_name) {
  switch (v->type) {
    case IR_VALUE_IMMEDIATE: {
      fprintf(out, "\tmov [%ld], %s\n", v->imm_i64, register_name);
    } break;

    case IR_VALUE_REGISTER:
    case IR_VALUE_ARGUMENT:
    case IR_VALUE_STATIC_ALLOCA: {
      int offset = calculate_offset(f, v);
      fprintf(out, "\tmov [rbp + %d], %s\n", offset, register_name);
    } break;

    case IR_VALUE_NOOP:
    case IR_VALUE_COUNT: {
      assert(0 && "This value should not be encountered");
    } break;

    default: {
      printf("%s\n", to_string(v->type));
      assert(0 && "not implemented");
    }
  }
}

void backend_emit_using_linux_x64_nasm_ld(IR_Hub *hub, const char *to) {
  char *asm_out = NULL;
  size_t asm_out_len = 0;
  auto out = open_memstream(&asm_out, &asm_out_len);

  fprintf(out, "\n\tsection .bss\n");
  fprintf(out, "integer_to_string_buffer resb %d\n", 21/*buffer for int64_t to string conversion*/);
  fprintf(out, "double_to_string_buffer resb %d\n", 80/*buffer for double to string conversion*/);

  fprintf(out, "\n\tsection .text\n");

  struct Callsites {
    const char *label;
    const char *location;
    uint32_t string_offset_in_array;
    uint32_t string_len_in_array;
  };
  Array<Callsites> callsites = {};

  FOR (hub->functions) {
    IR_Function *function = *it;
    if (function->builtin) {
      continue;
    }
    if (function->support_library) {
      fprintf(out, "\textern %s\n", function->name);
      continue;
    }

    auto read_into_register = [&](const char *register_name, IR_Value *v, int optional_offset = 0) {
      nasm_read_into_register(out, function, register_name, v, optional_offset);
    };

    auto write_from_register = [&](IR_Value *v, const char *register_name) {
      nasm_write_from_register(out, function, v, register_name);
    };

    fprintf(out, "\tglobal %s\n", function->name);
    fprintf(out, "%s:\n", function->name);

    fprintf(out, "\tpush rbp\n");
    fprintf(out, "\tmov rbp, rsp\n");
    fprintf(out, ".body:\n");
    // Alloca space for register temporaries
    fprintf(out, "\tsub rsp, %d\n", function->preamble_allocation_size);

    FOR (function->scratchpad.blocks) {
      IR_Block *block = *it;
      fprintf(out, ".%s:\n", block->name); // marking it with . at the beginning will make it local
      int i = 0;
      for (IR_Instruction *it = block->start_stub.next; it; it = it->next) {
      // FOR (block->instructions) {
      //   IR_Instruction *instruction = *it;
        IR_Instruction *instruction = it;
        fprintf(out, "\n");
        fprintf(out, "\t;.%s[%4d]", block->name, i);
        print_code_location(instruction->loc, out);
        fprintf(out, "\n");
        fprintf(out, "\t;.%s[%4d]\n", block->name, i);
        switch(instruction->type) {
        case IR_INSTRUCTION_NOOP:
        case IR_INSTRUCTION_COUNT:
          assert(0 && "this value should not be encountered");
          break;

        case IR_INSTRUCTION_SIGNED_INT_ADD: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tadd rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_SUB: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tsub rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        //signed and unsigned need different implementations here
        case IR_INSTRUCTION_SIGNED_INT_MUL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\timul rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        //signed and unsigned need different implementations here
        case IR_INSTRUCTION_SIGNED_INT_DIV: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          //fprintf(out, "\tmov rdx, 0\n");
          fprintf(out, "\tcqo\n");
          fprintf(out, "\tidiv rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        //signed and unsigned need different implementations here
        case IR_INSTRUCTION_SIGNED_INT_REMAINDER: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          //fprintf(out, "\tmov rdx, 0\n");
          fprintf(out, "\tcqo\n");
          fprintf(out, "\tidiv rcx\n");
          write_from_register(instruction->res, "rdx");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_OR: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tor rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_AND: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tand rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\txor rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_LOGICAL_OR: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tcmp   rax, 0\n");
          fprintf(out, "\tsetne al\n");

          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp   rcx, 0\n");
          fprintf(out, "\tsetne cl\n");

          fprintf(out, "\tor rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_LOGICAL_AND: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tcmp   rax, 0\n");
          fprintf(out, "\tsetne al\n");

          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp   rcx, 0\n");
          fprintf(out, "\tsetne cl\n");

          fprintf(out, "\tand rax, rcx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tcmp   rax, 0\n");
          fprintf(out, "\tsetne al\n");
          fprintf(out, "\tmov rbx, 1\n");
          fprintf(out, "\tsub bl, al\n");
          write_from_register(instruction->res, "rbx");
        } break;

        case IR_INSTRUCTION_RET: {
          if (instruction->op1) {
            read_into_register("rax", instruction->op1);
          }

          // restoring back stack pointer
          fprintf(out, "\tadd rsp, %d\n", function->preamble_allocation_size);
          fprintf(out, "\tpop rbp\n");
          fprintf(out, "\tret\n");
        } break;

        case IR_INSTRUCTION_CALL: {
          bool is_c_call = false;
          bool returns_float = false;

          IR_Function *callee = instruction->op1->function;
          bool call_directly_by_name = callee;
          if (call_directly_by_name) {
            is_c_call = instruction->op1->function->c_call;
            returns_float = instruction->op1->function->returns_float;
          }

          if (is_c_call) {
            assert(callee);
            const char *available_int_arg_registers[] = {
              "rdi", "rsi", "rdx", "rcx", "r8", "r9",
            };

            int int_args_used = 0;
            int float_args_used = 0;

            const char *available_float_arg_registers[] = {
              "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7",
            };

            for (int i = 0; i < instruction->call_args.len; ++i) {
              IR_Value *arg = instruction->call_args[i];
              read_into_register("rax", arg);
              if (callee->args[i]->is_floating_point) {
                assert(float_args_used < (int)C_ARRAY_LEN(available_float_arg_registers));
                fprintf(out, "\tmovq %s, rax\n", available_float_arg_registers[float_args_used]);
                float_args_used++;
              } else {
                assert(int_args_used < (int)C_ARRAY_LEN(available_int_arg_registers));
                fprintf(out, "\tmov %s, rax\n", available_int_arg_registers[int_args_used]);
                int_args_used++;
              }
            }
          } else {
            for (int i = 0; i < instruction->call_args.len; ++i) {
              IR_Value *arg = instruction->call_args[i];
              read_into_register("rax", arg);
              fprintf(out, "\tmov QWORD [rsp - %d], rax\n", 8 * (i + 3 + 1/*skip closure pointer*/));
            }
          }

          if (call_directly_by_name) {
            assert(instruction->closure == NULL);
            fprintf(out, "\tcall %s\n", callee->name);
          } else {
            if (instruction->closure) {
              read_into_register("rax", instruction->closure);
              fprintf(out, "\tmov QWORD [rsp - %d], rax\n", 8 * 3);
            }
            read_into_register("rax", instruction->op1);
            fprintf(out, "\tcall rax\n");
          }

          {
            // Saving callsite return addresses for nice stacktrace
            assert(instruction->loc);
            auto code_location = decode_code_location(*instruction->loc);
            auto location_string = alloc_sprintf("%s:%d:%d %s", code_location.file_name, code_location.line, code_location.col, function->language_level_name);
            fprintf(out, ".callsite_%d:\n", callsites.len);
            callsites.append({
              .label = alloc_sprintf("%s.callsite_%d", function->name, callsites.len),
              .location = location_string,
            });
          }

          if (is_c_call && returns_float) {
            fprintf(out, "\tmovq rax, xmm0\n");
          }
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SYSCALL: {
                                              read_into_register("rax", instruction->call_args[0]);
          if (instruction->call_args.len > 1) read_into_register("rdi", instruction->call_args[1]);
          if (instruction->call_args.len > 2) read_into_register("rsi", instruction->call_args[2]);
          if (instruction->call_args.len > 3) read_into_register("rdx", instruction->call_args[3]);
          assert(instruction->call_args.len <= 4);

          fprintf(out, "\tsyscall\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FILL_WITH_ZEROES: {
          assert(instruction->op2->type == IR_VALUE_IMMEDIATE);
          for (int i = 0; i < instruction->op2->imm_i64; ++i) {
            // TODO: use WORD, DWORD, QWORD to zero out value in less instructions
            read_into_register("rax", instruction->op1, i);
            fprintf(out, "\tmov BYTE [rax], 0\n");
          }
        } break;

        case IR_INSTRUCTION_COPY_BYTES: {
          assert(instruction->op3);
          assert(instruction->op3->type == IR_VALUE_IMMEDIATE);
          fprintf(out, "\t;COPY_BYTES\n");
          read_into_register("rax", instruction->op1); // to
          read_into_register("rcx", instruction->op2); // from
          for (int i = 0; i < instruction->op3->imm_i64; ++i) { // TODO utilize bigger chunks
            fprintf(out, "\tmov dl, byte [rcx + %d]\n", i);
            fprintf(out, "\tmov byte [rax + %d], dl\n", i);
          }
        } break;

        case IR_INSTRUCTION_64BIT_LOAD: {
          fprintf(out, "\t;LOAD\n");
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tmov rax, [rax]\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_64BIT_STORE: {
          fprintf(out, "\t;LOAD\n");
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmov [rax], rcx\n");
        } break;

        case IR_INSTRUCTION_COND_JUMP: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tjne .%s\n", instruction->then_block->name);
          fprintf(out, "\tjmp .%s\n", instruction->else_block->name);
        } break;

        case IR_INSTRUCTION_JUMP: {
          fprintf(out, "\tjmp .%s\n", instruction->then_block->name);
        } break;

        case IR_INSTRUCTION_VALUE_MERGE: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          read_into_register("rdx", instruction->op3);
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tcmovne rax, rcx\n");
          fprintf(out, "\tcmove rax, rdx\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_TAIL_CALL: {
          if (instruction->closure) {
            read_into_register("rax", instruction->closure);
            fprintf(out, "\tmov [rsp - %d], rax\n", 8);
          }
          for (int i = 0; i < instruction->call_args.len; ++i) {
            read_into_register("rax", instruction->call_args[i]);
            fprintf(out, "\tmov [rsp - %d], rax\n", 8 * (i + 1 + 1/*skip closure pointer*/));
          }
          if (instruction->closure) {
            fprintf(out, "\tmov rax, [rsp - %d]\n", 8);
            fprintf(out, "\tmov [rbp - %d], rax\n", 8);
          }
          for (int i = 0; i < instruction->call_args.len; ++i) {
            fprintf(out, "\tmov rax, [rsp - %d]\n", 8 * (i + 1 + 1/*skip closure pointer*/));
            fprintf(out, "\tmov [rbp - %d], rax\n", 8 * (i + 1 + 1/*skip closure pointer*/));
          }
          fprintf(out, "\tadd rsp, %d\n", function->preamble_allocation_size);

          if (instruction->op1->type == IR_VALUE_FUNCTION) {
            fprintf(out, "\tpop rbp\n");
            fprintf(out, "\tjmp %s\n", instruction->op1->function->name);
          } else {
            read_into_register("rax", instruction->op1);
            fprintf(out, "\tpop rbp\n");
            fprintf(out, "\tjmp rax\n");
          }
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsete al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        case IR_INSTRUCTION_SIGNED_INT_CMP_LESS: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetl al\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        case IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetle al\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetg al\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tcmp rax, rcx\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetge al\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        // docs says SAL and SHL do same thing
        case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tsal rax, cl\n");
          write_from_register(instruction->res, "rax");
        } break;

        // signed and unsignged ints need different impl
        case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tsar rax, cl\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_ADD: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\taddsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_SUB: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tsubsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_MUL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tmulsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_DIV: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tdivsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_LESS: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmpltsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmplesd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_GREATER: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmpnlesd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmpnltsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmpeqsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL: {
          read_into_register("rax", instruction->op1);
          read_into_register("rcx", instruction->op2);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tmovq xmm1, rcx\n");
          fprintf(out, "\tcmpneqsd xmm0, xmm1\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          fprintf(out, "\tcmp rax, 0\n");
          fprintf(out, "\tmov rax, 0\n");
          fprintf(out, "\tsetne al\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_CAST_FLOAT_TO_INTEGER_USING_TRUNCATION: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tmovq xmm0, rax\n");
          fprintf(out, "\tcvttsd2si rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        case IR_INSTRUCTION_CAST_INTEGER_TO_FLOAT: {
          read_into_register("rax", instruction->op1);
          fprintf(out, "\tcvtsi2sd xmm0, rax\n");
          fprintf(out, "\tmovq rax, xmm0\n");
          write_from_register(instruction->res, "rax");
        } break;

        }
        fprintf(out, "\t;--\n");
        i++;
      }
    }
    fprintf(out, "\n");
  }

  const char *entry_point_name = NULL;
  FOR (hub->functions) {
    IR_Function *f = *it;
    if (f->entry_point) {
      if (entry_point_name) {
        assert(0 && "multip entry points");
      } else {
        entry_point_name = f->name;
      }
    }
  }

  fprintf(out, "\tglobal _start\n");
  fprintf(out, "_start:\n");
  fprintf(out, "\tcall      %s\n", entry_point_name);
  fprintf(out, "\tmov       rdi, rax\n");
  fprintf(out, "\tmov       rax, 60\n");
  fprintf(out, "\tsyscall\n");
  fprintf(out, "\n");

  fprintf(out, "\n\tsection .rodata\n");

  FOR (hub->const_strings_structs) {
    IR_Value *v = *it;
    fprintf(out, "%s_data:\tdb ", v->name);
    bool first = true;
    for (size_t i = 0; i < v->len; ++i) {
      if (!first) fprintf(out, ", ");
      first = false;
      fprintf(out, "%d", v->data[i]);
    }

    int rest_to_fill_with_zeroes = 16 - v->len % 16;
    for (int i = 0; i < rest_to_fill_with_zeroes; i++) {
      if (!first) fprintf(out, ", ");
      first = false;
      fprintf(out, "0");
    }
    fprintf(out, "\n");
    fprintf(out, "\tglobal %s\n", v->name);
    fprintf(out, "%s:\tdq %s_data, %lu\n", v->name, v->name, v->len);
  }

  FOR (hub->const_globals) {
    IR_Value *v = *it;
    fprintf(out, "\tglobal %s\n", v->name);
    fprintf(out, "%s:\tdb ", v->name);
    bool first = true;
    for (size_t i = 0; i < v->len; ++i) {
      if (!first) fprintf(out, ", ");
      first = false;
      fprintf(out, "%d", v->data[i]);
    }
    fprintf(out, "\n");
  }

  {
    fprintf(out, "\tglobal __callsites_strings\n");
    fprintf(out, "__callsites_strings:\t\n");

    int offset = 0;
    FOR (callsites) {
      fprintf(out, "\tdb ");
      bool first = true;
      int len = 0;

      it->string_offset_in_array = offset;

      for (const char *char_it = it->location; *char_it; char_it++) {
        if (!first) fprintf(out, ", ");
        first = false;
        fprintf(out, "%d", (int)*char_it);

        offset++;
        len++;
      }
      fprintf(out, ";%s\n", it->location);

      it->string_len_in_array = len;
    }

    while (offset % 16 != 0) {
      fprintf(out, "\tdb 0 ; padding\n");
      offset++;
    }

    fprintf(out, "\tglobal __callsites_array_begin\n");
    fprintf(out, "\tglobal __callsites_array_end\n");
    fprintf(out, "__callsites_array_begin:\t\n");

    FOR (callsites) {
      fprintf(out, "\t; %s\n", it->location);
      fprintf(out, "\tdq %s\n", it->label);
      fprintf(out, "\tdd %d\n", it->string_offset_in_array);
      fprintf(out, "\tdd %d\n", it->string_len_in_array);
      fprintf(out, "\n");
    }

    fprintf(out, "__callsites_array_end:\t\n");
  }

  fclose(out);


  {
    auto file_out = fopen(alloc_sprintf("%s.asm", to), "w");
    if (!file_out) {
      report_error({}, "Failed to open asm file for writing");
      exit(1);
    }
    //fwrite(asm_out, asm_out_len, 1, stdout);
    fwrite(asm_out, asm_out_len, 1, file_out);
    fclose(file_out);
    free(asm_out);
  }

  {
    auto support_library_o_out = fopen("support_library.o", "wb");
    if (!support_library_o_out) {
      report_error({}, "Failed to open support_library.o for writing");
      exit(1);
    }
    fwrite(support_library_o_data, support_library_o_len, 1, support_library_o_out);
    fclose(support_library_o_out);
  }


  int status = 0;
  status = system(alloc_sprintf("nasm -felf64 -O0 -l %s.lst %s.asm -o %s.o", to, to, to));
  if (status) {
    report_error({}, "nasm call returned %d\n", status);
    exit(1);
  }
  status = system(alloc_sprintf("ld -znoexecstack -x %s.o support_library.o -o %s", to, to));
  if (status) {
    report_error({}, "ld call returned %d\n", status);
    exit(1);
  }
}
