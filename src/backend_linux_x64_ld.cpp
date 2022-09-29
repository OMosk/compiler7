#include "compiler.hpp"

#include <stdlib.h>

/*
 64-Bit Data Types

Name           Size  Alignment  Purpose
Elf64_Addr     8     8          Unsigned program address
Elf64_Off      8     8          Unsigned file offset
Elf64_Half     2     2          Unsigned medium integer
Elf64_Word     4     4          Unsigned integer
Elf64_Sword    4     4          Signed integer
Elf64_Xword    8     8          Unsigned long integer
Elf64_Sxword   8     8          Signed long integer
unsigned char  1     1          Unsigned small integer

#define EI_NIDENT 16

typedef struct {
        unsigned char   e_ident[EI_NIDENT];
        Elf64_Half      e_type;
        Elf64_Half      e_machine;
        Elf64_Word      e_version;
        Elf64_Addr      e_entry;
        Elf64_Off       e_phoff;
        Elf64_Off       e_shoff;
        Elf64_Word      e_flags;
        Elf64_Half      e_ehsize;
        Elf64_Half      e_phentsize;
        Elf64_Half      e_phnum;
        Elf64_Half      e_shentsize;
        Elf64_Half      e_shnum;
        Elf64_Half      e_shstrndx;
} Elf64_Ehdr;

typedef struct {
	Elf64_Word	sh_name;
	Elf64_Word	sh_type;
	Elf64_Xword	sh_flags;
	Elf64_Addr	sh_addr;
	Elf64_Off	sh_offset;
	Elf64_Xword	sh_size;
	Elf64_Word	sh_link;
	Elf64_Word	sh_info;
	Elf64_Xword	sh_addralign;
	Elf64_Xword	sh_entsize;
} Elf64_Shdr;

typedef struct {
	Elf64_Word	st_name;
	unsigned char	st_info;
	unsigned char	st_other;
	Elf64_Half	st_shndx;
	Elf64_Addr	st_value;
	Elf64_Xword	st_size;
} Elf64_Sym;

typedef struct {
	Elf64_Addr	r_offset;
	Elf64_Xword	r_info;
	Elf64_Sxword	r_addend;
} Elf64_Rela;

#define ELF64_R_SYM(i)    ((i)>>32)
#define ELF64_R_TYPE(i)   ((i)&0xffffffffL)
#define ELF64_R_INFO(s,t) (((s)<<32)+((t)&0xffffffffL))



https://refspecs.linuxfoundation.org/elf/gabi4+/ch4.eheader.html
*/

//
uint8_t modrm(uint8_t mod, uint8_t reg, uint8_t rm) {
  uint8_t result = (mod << 6) | (reg << 3) | rm; // TODO: to prevent from accidents &0b11 &0b111
  return result;
}

uint8_t sib(uint8_t scale, uint8_t index, uint8_t base) {
  uint8_t result = (scale << 6) | (index << 3) | base; // TODO: to prevent from accidents &0b11 &0b111
  return result;
}


void backend_emit_using_linux_x64_ld(IR_Hub *hub, const char *to) {
  enum X64_Register {
    RAX = 0,
    RCX = 1,
    RDX = 2,
    RBX = 3,
    RSP = 4,
    RBP = 5,
    RSI = 6,
    RDI = 7,
    R8  = 8,
    R9  = 9,
    R10 = 10,
    R11 = 11,
    R12 = 12,
    R13 = 13,
    R14 = 14,
    R15 = 15,

    XMM0 = 0,
    XMM1 = 1,
    XMM2 = 2,
    XMM3 = 3,
    XMM4 = 4,
    XMM5 = 5,
    XMM6 = 6,
    XMM7 = 7,
  };
  const int32_t REX_W = 0x40 | (1 << 3);
  const int32_t REX_B = 0x40 | (1 << 0);

  struct Compiled_Code {
    enum class Section {
      INVALID, TEXT, BSS, RODATA, EXTERN
    };
    enum class Kind {
      INVALID, FUNCTION, OBJECT, UNKNOWN
    };
    struct Relocation {
      const char *symbol_name;
      int64_t offset;
      int64_t addend;
      bool is_absolute;
    };

    struct Symbol {
      const char *name;
      Section section;
      Kind kind;
      int64_t offset, length;
    };

    Buffer text_section_buffer;
    Array<Relocation> text_section_relocations;

    Buffer rodata_section_buffer;
    Array<Relocation> rodata_section_relocations;

    Array<Symbol> symbols;

    int64_t entry_function_offset;
  };

  Compiled_Code compiled_code = {};

  const int symtab_section_number = 2;
  const int text_section_number = 3;
  const int rodata_section_number = 4;

  auto *t = &compiled_code.text_section_buffer;
  reserve(t, 1024);


  FOR (hub->const_strings_structs) {
    IR_Value *v = *it;

    Compiled_Code::Symbol symbol = {};
    auto data_symbol_name = symbol.name = alloc_sprintf("%s_data", v->name);
    symbol.section = Compiled_Code::Section::RODATA;
    symbol.kind = Compiled_Code::Kind::OBJECT;
    symbol.offset = compiled_code.rodata_section_buffer.len;
    symbol.length = v->len;

    compiled_code.symbols.append(symbol);

    for (size_t i = 0; i < v->len; ++i) {
      add_1b(&compiled_code.rodata_section_buffer, v->data[i]);
    }

    int rest_to_fill_with_zeroes = 16 - v->len % 16;
    for (int i = 0; i < rest_to_fill_with_zeroes; i++) {
      add_1b(&compiled_code.rodata_section_buffer, 0);
    }

    symbol.name = v->name;
    symbol.offset = compiled_code.rodata_section_buffer.len;
    symbol.length = 16; // hardcode
    compiled_code.symbols.append(symbol);

    auto relocation_offset = add_8b(&compiled_code.rodata_section_buffer, 0);
    add_8b(&compiled_code.rodata_section_buffer, v->len);

    compiled_code.rodata_section_relocations.append({.symbol_name = data_symbol_name, .offset = relocation_offset, .is_absolute = true});
  };

  struct Callsites {
    const char *function_symbol;
    uint32_t offset_in_function_code;
    const char *location;
    uint32_t string_offset_in_array;
    uint32_t string_len_in_array;
  };
  Array<Callsites> callsites = {};

  struct BlockOffsetPair {
    IR_Block *block;
    int32_t start_offset;
  };

  Array<BlockOffsetPair> block_offsets = {};

  struct BlockOffsetsToPatch {
    IR_Block *block;
    int32_t offset_to_patch;
  };
  Array<BlockOffsetsToPatch> jumps_to_patch = {};

  FOR (hub->functions) {
    block_offsets.len = 0; // reuse array;
    jumps_to_patch.len = 0; // reuse array;

    IR_Function *function = *it;
    if (function->entry_point) {
      compiled_code.entry_function_offset = compiled_code.text_section_buffer.len;
    }


    Compiled_Code::Symbol symbol = {};
    symbol.name = function->name;
    if (function->support_library) {
      symbol.section = Compiled_Code::Section::EXTERN;
      symbol.kind = Compiled_Code::Kind::UNKNOWN;
      compiled_code.symbols.append(symbol);
      continue;
    }

    symbol.section = Compiled_Code::Section::TEXT;
    symbol.kind = Compiled_Code::Kind::FUNCTION;


    if (function->builtin) {
      assert(0 && "not supported");
      continue;
    }

    auto read_into_register = [&] (X64_Register r, IR_Value *v, int optional_offset = 0) {
      switch (v->type) {
        case IR_VALUE_IMMEDIATE: {
          assert(optional_offset == 0);
          // REX.W + B8+ rd io |	MOV r64, imm64
          assert((int)r < 8);
          add_1b(t, 0x40 | (1 << 3)); // REX.W
          add_1b(t, 0xB8 + (int) r);  // B8 + rd
          add_8b(t, v->imm_i64);
        } break;

        // case IR_VALUE_CONSTANT_GLOBAL: {
        //   assert(optional_offset == 0);
        //   fprintf(f, "\tmov %s, %s\n", register_name, v->name);
        // } break;

        case IR_VALUE_FUNCTION:
        case IR_VALUE_CONSTANT_STRING_STRUCT: {
          assert(optional_offset == 0);
          //REX.W + B8+ rd io |	MOV r64, imm64
          assert((int)r < 8);
          add_1b(t, REX_W); // REX.W
          add_1b(t, 0xB8 + (int) r);  // B8 + rd
          auto offset_to_patch = add_8b(t, 0);
          compiled_code.text_section_relocations.append({.symbol_name = v->name, .offset = offset_to_patch, .is_absolute = true});
          //fprintf(f, "\tmov %s, %s\n", register_name, v->name);
        } break;

        case IR_VALUE_ARGUMENT:
        case IR_VALUE_REGISTER: {
          int offset = calculate_offset(function, v);

          //REX.W + 8B /r 	MOV r64,r/m64
          add_1b(t, REX_W);
          add_1b(t, 0x8B);
          add_1b(t, modrm(0b10, r, 0b101/*[rbp]*/));
          add_4b(t, offset);

          if (optional_offset) {
            //REX.W + 05 id 	ADD RAX, imm32
            add_1b(t, REX_W);
            add_1b(t, 0x05);
            add_4b(t, optional_offset);
          }
        } break;

        // case IR_VALUE_ARGUMENT: {
        //   assert(optional_offset == 0);
        //   int offset = calculate_offset(function, v);
        //   fprintf(f, "\tmov %s, [rbp + %d]\n", register_name, offset);
        // } break;
        // case IR_VALUE_STATIC_ALLOCA: {
        //   int offset = calculate_offset(function, v) + optional_offset;
        //   fprintf(f, "\tlea %s, [rbp + %d]\n", register_name, offset);
        // } break;

        // case IR_VALUE_FUNCTION: {
        //   assert(optional_offset == 0);
        //   fprintf(f, "\tmov %s, %s\n", register_name, v->name);
        // } break;

        case IR_VALUE_CLOSURE_BASE: {
          assert((int)r < 8);
          //fprintf(f, "\tmov %s, [rbp - 8]\n", register_name);
          // REX.W + 8B /r 	MOV r64,r/m64
          add_1b(t, REX_W);
          add_1b(t, 0x8B);
          add_1b(t, modrm(0b01, r, X64_Register::RBP));
          add_1b(t, -8);
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
    };
    auto write_from_register = [&] (IR_Value *v, X64_Register r) {
      auto f = function;
      switch (v->type) {
        // case IR_VALUE_IMMEDIATE: {
        //   fprintf(out, "\tmov [%ld], %s\n", v->imm_i64, register_name);
        // } break;

        case IR_VALUE_REGISTER:
        case IR_VALUE_ARGUMENT:
        case IR_VALUE_STATIC_ALLOCA: {
          int offset = calculate_offset(f, v);

          //REX.W + 89 /r 	MOV r/m64,r64
          add_1b(t, REX_W);
          add_1b(t, 0x89);

          // mod 10 - 32 bit displacement
          add_1b(t, modrm(0b10, r, 0b101/*[rbp]*/));
          add_4b(t, offset);

          //fprintf(out, "\tmov [rbp + %d], %s\n", offset, register_name);
        } break;

        // case IR_VALUE_NOOP:
        // case IR_VALUE_COUNT: {
        //   assert(0 && "This value should not be encountered");
        // } break;

        default: {
          printf("%s\n", to_string(v->type));
          assert(0 && "not implemented");
        }
      }
    };

    auto floating_point_cmp = [&] (IR_Instruction *instruction, uint8_t cmp_byte) {
      read_into_register(X64_Register::RAX, instruction->op1);
      read_into_register(X64_Register::RCX, instruction->op2);

      // movq xmm0, rax\n");
      // movq xmm1, rcx\n");
      // // F2 0F C2 /r ib CMPSD xmm1, xmm2/m64, imm8
      // cmpltsd xmm0, xmm1\n");
      // movq rax, xmm0\n");
      // cmp rax, 0\n");
      // mov rax, 0\n");
      // setne al\n");

      // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
      add_1b(t, 0x66);
      add_1b(t, REX_W);
      add_1b(t, 0x0F);
      add_1b(t, 0x6E);
      add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

      add_1b(t, 0x66);
      add_1b(t, REX_W);
      add_1b(t, 0x0F);
      add_1b(t, 0x6E);
      add_1b(t, modrm(0b11, X64_Register::XMM1, X64_Register::RCX));
      //------------------------------------------------------------

      // Actual work
      // F2 0F C2 /r ib CMPSD xmm1, xmm2/m64, imm8
      add_1b(t, 0xF2);
      add_1b(t, 0x0F);
      add_1b(t, 0xC2);
      add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::XMM1));
      add_1b(t, cmp_byte);

      //------------------------------------------------------------
      //66 REX.W 0F 7E /r MOVQ r/m64, xmm
      add_1b(t, 0x66);
      add_1b(t, REX_W);
      add_1b(t, 0x0F);
      add_1b(t, 0x7E);
      add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

      //REX.W + 3D id 	CMP RAX, imm32
      add_1b(t, REX_W);
      add_1b(t, 0x3D);
      add_4b(t, 0);

      //REX.W + C7 /0 id 	MOV r/m64, imm32
      add_1b(t, REX_W);
      add_1b(t, 0xC7);
      add_1b(t, modrm(0b11, 0, X64_Register::RAX));
      add_4b(t, 0);

      //0F 95 	SETNE r/m8
      add_1b(t, 0x0F);
      add_1b(t, 0x95);
      add_1b(t, modrm(0b11, 0, X64_Register::RAX));

      write_from_register(instruction->res, X64_Register::RAX);
    };


    auto signed_int_cmp = [&] (IR_Instruction *instruction, uint8_t opcode_byte) {
      read_into_register(X64_Register::RAX, instruction->op1);
      read_into_register(X64_Register::RCX, instruction->op2);

      // REX.W + 3B /r 	CMP r64, r/m64
      // cmp rax, rcx
      add_1b(t, REX_W);
      add_1b(t, 0x3B);
      add_1b(t, modrm(0b11, X64_Register::RAX, X64_Register::RCX));

      // REX.W + C7 /0 id 	MOV r/m64, imm32 	MI 	Valid 	N.E. 	Move imm32 sign extended to 64-bits to r/m64.
      // mov rax, 0
      add_1b(t, REX_W);
      add_1b(t, 0xC7);
      add_1b(t, modrm(0b11, 0, X64_Register::RAX));
      add_4b(t, 0);

      // setne al
      // 0F 95 SETNE r/m8
      add_1b(t, 0x0F);
      add_1b(t, opcode_byte);
      add_1b(t, modrm(0b11, 0, X64_Register::RAX));

      write_from_register(instruction->res, X64_Register::RAX);
    };

    auto signed_int_arith = [&] (IR_Instruction *instruction, uint8_t opcode_byte, uint8_t second_opcode_byte = 0) {
      read_into_register(X64_Register::RAX, instruction->op1);
      read_into_register(X64_Register::RCX, instruction->op2);

      // REX.W + 03 /r 	ADD r64, r/m64
      add_1b(t, REX_W);
      add_1b(t, opcode_byte);
      if (second_opcode_byte) add_1b(t, second_opcode_byte);
      add_1b(t, modrm(0b11, X64_Register::RAX, X64_Register::RCX));

      write_from_register(instruction->res, X64_Register::RAX);
    };

    symbol.offset = compiled_code.text_section_buffer.len;
    // Preamble
    {
      add_1b(t, 0x55); // push rbp

      // mov rbp, rsp
      add_1b(t, 0x48);
      add_1b(t, 0x89);
      add_1b(t, 0xE5);

      // sub rsp, %d
      // REX.W + 81 /5 id
      add_1b(t, 0x40 | (1 << 3)); // REX.W
      add_1b(t, 0x81); // opcode sub
      add_1b(t, modrm(0b11, 5/*opcode extension*/, 0b100/*rsp*/)); // ModRM
      add_4b(t, function->preamble_allocation_size);
      //assert(function->preamble_allocation_size);
    }

    FOR (function->scratchpad.blocks) {
      IR_Block *block = *it;

      // Save where current block starts;
      block_offsets.append({block, compiled_code.text_section_buffer.len});

      for (IR_Instruction *it = block->start_stub.next; it; it = it->next) {
        IR_Instruction *instruction = it;

        switch(instruction->type) {
        default: assert(0 && "ir instruction not handled in x64 backend");
        case IR_INSTRUCTION_NOOP:
        case IR_INSTRUCTION_COUNT:
          assert(0 && "this value should not be encountered");
          break;

        case IR_INSTRUCTION_SIGNED_INT_LOGICAL_NOT: {
          read_into_register(X64_Register::RAX, instruction->op1);

          //fprintf(out, "\tcmp   rax, 0\n");
          //REX.W + 3D id 	CMP RAX, imm32
          add_1b(t, REX_W);
          add_1b(t, 0x3D);
          add_4b(t, 0);

          // setne al
          // 0F 95 SETNE r/m8
          add_1b(t, 0x0F);
          add_1b(t, 0x95);
          add_1b(t, modrm(0b11, 0, X64_Register::RAX));

          // fprintf(out, "\tmov rbx, 1\n");
          add_1b(t, REX_W); // REX.W
          add_1b(t, 0xB8 + X64_Register::RBX);  // B8 + rd
          add_8b(t, 1);

          //fprintf(out, "\tsub bl, al\n");
          //2A /r 	SUB r8, r/m8
          add_1b(t, 0x2A);
          add_1b(t, modrm(0b11, X64_Register::RBX/*bl*/, X64_Register::RAX/*al*/));

          write_from_register(instruction->res, X64_Register::RBX);
        } break;

        case IR_INSTRUCTION_COPY_BYTES: {
          assert(instruction->op3);
          assert(instruction->op3->type == IR_VALUE_IMMEDIATE);

          read_into_register(X64_Register::RAX, instruction->op1); // to
          read_into_register(X64_Register::RCX, instruction->op2); // from

          assert(instruction->op3->imm_i64 <= 255);
          for (int i = 0; i < instruction->op3->imm_i64; ++i) { // TODO utilize bigger chunks
            // 8A /r 	MOV r8,r/m8
            //fprintf(out, "\tmov dl, byte [rcx + %d]\n", i);
            add_1b(t, 0x8A);
            add_1b(t, modrm(0b01, X64_Register::RDX/*dl*/, X64_Register::RCX));
            add_1b(t, i);

            //88 /r 	MOV r/m8,r8
            //fprintf(out, "\tmov byte [rax + %d], dl\n", i);
            add_1b(t, 0x88);
            add_1b(t, modrm(0b01, X64_Register::RDX/*dl*/, X64_Register::RAX));
            add_1b(t, i);
          }
        } break;

        case IR_INSTRUCTION_CAST_INTEGER_TO_FLOAT: {
          read_into_register(X64_Register::RAX, instruction->op1);

          // fprintf(out, "\tcvtsi2sd xmm0, rax\n");
          // F2 REX.W 0F 2A /r CVTSI2SD xmm1, r/m64
          add_1b(t, 0xF2);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x2A);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          //fprintf(out, "\tmovq rax, xmm0\n");
          //66 REX.W 0F 7E /r MOVQ r/m64, xmm
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x7E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_CAST_FLOAT_TO_INTEGER_USING_TRUNCATION: {
          read_into_register(X64_Register::RAX, instruction->op1);
          // fprintf(out, "\tmovq xmm0, rax\n");
          // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          //fprintf(out, "\tcvttsd2si rax, xmm0\n");
          //F2 REX.W 0F 2C /r CVTTSD2SI r64, xmm1/m64
          add_1b(t, 0xF2);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x2C);
          add_1b(t, modrm(0b11, X64_Register::RAX, X64_Register::XMM0));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_VALUE_MERGE: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);
          read_into_register(X64_Register::RDX, instruction->op3);
          // REX.W + 3D id 	CMP RAX, imm32
          // cmp rax, 0
          add_1b(t, REX_W);
          add_1b(t, 0x3D);
          add_4b(t, 0);

          // mov rax, rcx
          // REX.W + 8B /r 	MOV r64,r/m64
          add_1b(t, REX_W);
          add_1b(t, 0x8B);
          add_1b(t, modrm(0b11, X64_Register::RAX, X64_Register::RCX));
          //fprintf(out, "\tcmovne rax, rcx\n");


          //REX.W + 0F 44 /r 	CMOVE r64, r/m64
          //fprintf(out, "\tcmove rax, rdx\n");
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x44);
          add_1b(t, modrm(0b11, X64_Register::RAX, X64_Register::RDX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_JUMP: {
          //E9 cd 	JMP rel32
          add_1b(t, 0xE9);
          auto block_offset_to_patch = add_4b(t, 0);
          jumps_to_patch.append({.block = instruction->then_block, .offset_to_patch = block_offset_to_patch});
        } break;

        case IR_INSTRUCTION_COND_JUMP: {
          read_into_register(X64_Register::RAX, instruction->op1);
          // REX.W + 3D id 	CMP RAX, imm32
          // cmp rax, 0
          add_1b(t, REX_W);
          add_1b(t, 0x3D);
          add_4b(t, 0);

          //0F 85 cd 	JNE rel32
          add_1b(t, 0x0F);
          add_1b(t, 0x85);
          auto then_block_offset_to_patch = add_4b(t, 0);
          jumps_to_patch.append({.block = instruction->then_block, .offset_to_patch = then_block_offset_to_patch});

          //E9 cd 	JMP rel32
          add_1b(t, 0xE9);
          auto else_block_offset_to_patch = add_4b(t, 0);
          jumps_to_patch.append({.block = instruction->else_block, .offset_to_patch = else_block_offset_to_patch});
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_EQUAL: {
          // 0F 94 	SETE r/m8
          signed_int_cmp(instruction, 0x94);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_NOT_EQUAL: {
          // setne al
          // 0F 95 SETNE r/m8
          signed_int_cmp(instruction, 0x95);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_LESS: {
          // REX + 0F 9C 	SETL r/m8*
          signed_int_cmp(instruction, 0x9C);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_LESS_OR_EQUAL: {
          // 0F 9E 	SETLE r/m8
          signed_int_cmp(instruction, 0x9E);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER: {
          // 0F 9F 	SETG r/m8
          signed_int_cmp(instruction, 0x9F);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_CMP_GREATER_OR_EQUAL: {
          // 0F 9D 	SETGE r/m8
          signed_int_cmp(instruction, 0x9D);
        } break;

        case IR_INSTRUCTION_FILL_WITH_ZEROES: {
          assert(instruction->op2->type == IR_VALUE_IMMEDIATE);
          for (int i = 0; i < instruction->op2->imm_i64; ++i) {
            // TODO: use WORD, DWORD, QWORD to zero out value in less instructions
            read_into_register(X64_Register::RAX, instruction->op1, i);
            // fprintf(out, "\tmov BYTE [rax], 0\n");
            // REX + C6 /0 ib 	MOV r/m8***, imm8
            //add_1b(t, REX_W);
            add_1b(t, 0xC6);
            add_1b(t, modrm(0, 0, X64_Register::RAX));
            add_1b(t, 0);
          }
        } break;

        case IR_INSTRUCTION_SYSCALL: {
                                              read_into_register(X64_Register::RAX, instruction->call_args[0]);
          if (instruction->call_args.len > 1) read_into_register(X64_Register::RDI, instruction->call_args[1]);
          if (instruction->call_args.len > 2) read_into_register(X64_Register::RSI, instruction->call_args[2]);
          if (instruction->call_args.len > 3) read_into_register(X64_Register::RDX, instruction->call_args[3]);
          assert(instruction->call_args.len <= 4);

          //0F 05 SYSCALL
          //fprintf(out, "\tsyscall\n");
          add_1b(t, 0x0F);
          add_1b(t, 0x05);
          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_64BIT_STORE: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          //REX.W + 89 /r 	MOV r/m64,r64
          //mov [rax], rcx
          add_1b(t, REX_W);
          add_1b(t, 0x89);
          add_1b(t, modrm(0, X64_Register::RCX, X64_Register::RAX));
        } break;

        case IR_INSTRUCTION_64BIT_LOAD: {
          read_into_register(X64_Register::RAX, instruction->op1);

          //REX.W + 8B /r 	MOV r64, r/m64
          // mov rax, [rax]
          add_1b(t, REX_W);
          add_1b(t, 0x8B);
          add_1b(t, modrm(0b00, X64_Register::RAX, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_RET: {
          if (instruction->op1) {
            read_into_register(X64_Register::RAX, instruction->op1);
          }

          // add rsp, %d
          // REX.W + 81 /0 id 	ADD r/m64, imm32
          add_1b(t, 0x40 | (1 << 3)); // REX.W
          add_1b(t, 0x81); // 81
          add_1b(t, modrm(0b11, 0/*opcode extension*/, 0b100/*rsp*/)); // ModRM
          add_4b(t, function->preamble_allocation_size);
          assert(function->preamble_allocation_size);

          // pop rbp
          add_1b(t, 0x5D);

          // ret
          add_1b(t, 0xC3);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_ADD: {
          // REX.W + 03 /r 	ADD r64, r/m64
          signed_int_arith(instruction, 0x03);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_SUB: {
          // REX.W + 2B /r 	SUB r64, r/m64
          signed_int_arith(instruction, 0x2B);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_MUL: {
          // REX.W + 0F AF /r 	IMUL r64, r/m64
          signed_int_arith(instruction, 0x0F, 0xAF);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_DIV: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          // CQO
          add_1b(t, REX_W);
          add_1b(t, 0x99);

          // REX.W + F7 /7 	IDIV r/m64

          add_1b(t, REX_W);
          add_1b(t, 0xF7);
          add_1b(t, modrm(0b11, 7, X64_Register::RCX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_REMAINDER: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          // CQO
          add_1b(t, REX_W);
          add_1b(t, 0x99);

          // REX.W + F7 /7 	IDIV r/m64
          add_1b(t, REX_W);
          add_1b(t, 0xF7);
          add_1b(t, modrm(0b11, 7, X64_Register::RCX));

          write_from_register(instruction->res, X64_Register::RDX);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_OR: {
          // REX.W + 0B /r 	OR r64, r/m64
          signed_int_arith(instruction, 0x0B);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_AND: {
          // REX.W + 23 /r 	AND r64, r/m64
          signed_int_arith(instruction, 0x23);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_XOR: {
          // REX.W + 33 /r 	XOR r64, r/m64
          signed_int_arith(instruction, 0x33);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_LEFT: {
          // REX.W + D3 /4 	SAL r/m64, CL
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          add_1b(t, REX_W);
          add_1b(t, 0xD3);
          add_1b(t, modrm(0b11, 4, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_SIGNED_INT_BITWISE_SHIFT_RIGHT: {
          // REX.W + D3 /7 	SAR r/m64, CL
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          add_1b(t, REX_W);
          add_1b(t, 0xD3);
          add_1b(t, modrm(0b11, 7, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_FLOAT_ADD: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          //tmovq xmm0, rax
          //tmovq xmm1, rcx
          //taddsd xmm0, xmm1
          //tmovq rax, xmm0

          // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM1, X64_Register::RCX));

          // Actual work
          // F2 0F 58 /r ADDSD xmm1, xmm2/m64
          add_1b(t, 0xF2);
          add_1b(t, 0x0F);
          add_1b(t, 0x58);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::XMM1));

          //66 REX.W 0F 7E /r MOVQ r/m64, xmm
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x7E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_FLOAT_SUB: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          //tmovq xmm0, rax
          //tmovq xmm1, rcx
          //tsubsd xmm0, xmm1
          //tmovq rax, xmm0

          // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM1, X64_Register::RCX));

          // Actual work
          // F2 0F 5C /r SUBSD xmm1, xmm2/m64
          add_1b(t, 0xF2);
          add_1b(t, 0x0F);
          add_1b(t, 0x5C);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::XMM1));

          //66 REX.W 0F 7E /r MOVQ r/m64, xmm
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x7E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_FLOAT_MUL: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          //movq xmm0, rax
          //movq xmm1, rcx
          //mulsd xmm0, xmm1
          //movq rax, xmm0

          // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM1, X64_Register::RCX));

          // Actual work
          // F2 0F 59 /r MULSD xmm1,xmm2/m64
          add_1b(t, 0xF2);
          add_1b(t, 0x0F);
          add_1b(t, 0x59);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::XMM1));

          //66 REX.W 0F 7E /r MOVQ r/m64, xmm
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x7E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_FLOAT_DIV: {
          read_into_register(X64_Register::RAX, instruction->op1);
          read_into_register(X64_Register::RCX, instruction->op2);

          //movq xmm0, rax
          //movq xmm1, rcx
          //divsd xmm0, xmm1
          //movq rax, xmm0

          // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x6E);
          add_1b(t, modrm(0b11, X64_Register::XMM1, X64_Register::RCX));

          // Actual work
          // F2 0F 5E /r DIVSD xmm1, xmm2/m64
          add_1b(t, 0xF2);
          add_1b(t, 0x0F);
          add_1b(t, 0x5E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::XMM1));

          //66 REX.W 0F 7E /r MOVQ r/m64, xmm
          add_1b(t, 0x66);
          add_1b(t, REX_W);
          add_1b(t, 0x0F);
          add_1b(t, 0x7E);
          add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));

          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_LESS: {
          floating_point_cmp(instruction, 1/*less*/);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_LESS_OR_EQUAL: {
          floating_point_cmp(instruction, 2/*less or equal*/);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_EQUAL: {
          floating_point_cmp(instruction, 0/*equal*/);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_GREATER: {
          floating_point_cmp(instruction, 6/*nle*/);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_GREATER_OR_EQUAL: {
          floating_point_cmp(instruction, 5/*nlt*/);
        } break;

        case IR_INSTRUCTION_FLOAT_CMP_NOT_EQUAL: {
          floating_point_cmp(instruction, 4/*ne*/);
        } break;

        case IR_INSTRUCTION_TAIL_CALL: {
          if (instruction->closure) {
            read_into_register(X64_Register::RAX, instruction->closure);
            // REX.W + 89 /r 	MOV r/m64,r64
            add_1b(t, REX_W);
            add_1b(t, 0x89);
            add_1b(t, modrm(0b01, X64_Register::RAX, X64_Register::RSP/*SIB*/));
            add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b100 /*base [RSP]*/));
            add_1b(t, -8);
            //fprintf(out, "\tmov [rsp - %d], rax\n", 8);
          }
          for (int i = 0; i < instruction->call_args.len; ++i) {
            read_into_register(X64_Register::RAX, instruction->call_args[i]);
            //fprintf(out, "\tmov [rsp - %d], rax\n", 8 * (i + 1 + 1/*skip closure pointer*/));
            add_1b(t, REX_W);
            add_1b(t, 0x89);
            add_1b(t, modrm(0b10, X64_Register::RAX, X64_Register::RSP/*Acts as SIB*/));
            add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b100 /*base [RSP]*/));
            add_4b(t, -8 * (i + 1 + 1/*skip closure pointer*/));
          }
          if (instruction->closure) {
            // REX.W + 8B /r 	MOV r64,r/m64
            //fprintf(out, "\tmov rax, [rsp - %d]\n", 8);
            add_1b(t, REX_W);
            add_1b(t, 0x8B);
            add_1b(t, modrm(0b01, X64_Register::RAX, X64_Register::RSP/*SIB*/));
            add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b100 /*base [RSP]*/));
            add_1b(t, -8);

            // REX.W + 89 /r 	MOV r/m64,r64
            // fprintf(out, "\tmov [rbp - %d], rax\n", 8);
            add_1b(t, REX_W);
            add_1b(t, 0x89);
            add_1b(t, modrm(0b01, X64_Register::RAX, X64_Register::RBP));
            add_1b(t, -8);
          }
          for (int i = 0; i < instruction->call_args.len; ++i) {
            //fprintf(out, "\tmov rax, [rsp - %d]\n", 8 * (i + 1 + 1/*skip closure pointer*/));
            //fprintf(out, "\tmov [rbp - %d], rax\n", 8 * (i + 1 + 1/*skip closure pointer*/));

            // REX.W + 8B /r 	MOV r64,r/m64
            //fprintf(out, "\tmov rax, [rsp - %d]\n", 8);
            add_1b(t, REX_W);
            add_1b(t, 0x8B);
            add_1b(t, modrm(0b10, X64_Register::RAX, X64_Register::RSP/*SIB*/));
            add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b100 /*base [RSP]*/));
            add_4b(t, -8 * (i + 1 + 1/*skip closure pointer*/));

            // REX.W + 89 /r 	MOV r/m64,r64
            // fprintf(out, "\tmov [rbp - %d], rax\n", 8);
            add_1b(t, REX_W);
            add_1b(t, 0x89);
            add_1b(t, modrm(0b10, X64_Register::RAX, X64_Register::RBP));
            //add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b101 /*base [RBP]*/));
            add_4b(t, -8 * (i + 1 + 1/*skip closure pointer*/));
          }
          // REX.W + 81 /0 id 	ADD r/m64, imm32
          //fprintf(out, "\tadd rsp, %d\n", function->preamble_allocation_size);
          add_1b(t, REX_W);
          add_1b(t, 0x81);
          add_1b(t, modrm(0b11, 0, X64_Register::RSP));
          add_4b(t, function->preamble_allocation_size);

          if (instruction->op1->type == IR_VALUE_FUNCTION) {
            //fprintf(out, "\tpop rbp\n");
            // 58+ rd 	POP r64
            add_1b(t, 0x58 + X64_Register::RBP);

            //fprintf(out, "\tjmp %s\n", instruction->op1->function->name);
            //E9 cd 	JMP rel32
            add_1b(t, 0xE9);
            auto offset = add_4b(t, 0);

            compiled_code.text_section_relocations.append({
              .symbol_name = instruction->op1->function->name,
              .offset = offset,
              .addend = -4
            });
          } else {
            read_into_register(X64_Register::RAX, instruction->op1);
            //fprintf(out, "\tpop rbp\n");
            // 58+ rd 	POP r64
            add_1b(t, 0x58 + X64_Register::RBP);

            //fprintf(out, "\tjmp rax\n");
            //FF /4 	JMP r/m64
            add_1b(t, 0xFF);
            add_1b(t, modrm(0b11, 4, X64_Register::RAX));
          }
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
            X64_Register available_int_arg_registers[] = {
              X64_Register::RDI,
              X64_Register::RSI,
              X64_Register::RDX,
              X64_Register::RCX,
              X64_Register::R8,
              X64_Register::R9,
            };

            int int_args_used = 0;
            int float_args_used = 0;

            X64_Register available_float_arg_registers[] = {
              X64_Register::XMM0,
              X64_Register::XMM1,
              X64_Register::XMM2,
              X64_Register::XMM3,
              X64_Register::XMM4,
              X64_Register::XMM5,
              X64_Register::XMM6,
              X64_Register::XMM7,
            };

            for (int i = 0; i < instruction->call_args.len; ++i) {
              IR_Value *arg = instruction->call_args[i];
              read_into_register(X64_Register::RAX, arg);
              if (callee->args[i]->is_floating_point) {
                assert(float_args_used < (int)C_ARRAY_LEN(available_float_arg_registers));
                // 66 REX.W 0F 6E /r MOVQ xmm, r/m64
                add_1b(t, 0x66);
                add_1b(t, REX_W);
                add_1b(t, 0x0F);
                add_1b(t, 0x6E);
                add_1b(t, modrm(0b11, available_float_arg_registers[float_args_used], X64_Register::RAX));

                //fprintf(out, "\tmovq %s, rax\n", available_float_arg_registers[float_args_used]);
                float_args_used++;
              } else {
                assert(int_args_used < (int)C_ARRAY_LEN(available_int_arg_registers));

                auto next_register_to_use = available_int_arg_registers[int_args_used];
                //fprintf(out, "\tmov %s, rax\n", available_int_arg_registers[int_args_used]);
                //REX.W + 89 /r 	MOV r/m64,r64
                add_1b(t, REX_W | (REX_B * (next_register_to_use >= 8)));
                add_1b(t, 0x89);
                add_1b(t, modrm(0b11, X64_Register::RAX, next_register_to_use & 7));
                int_args_used++;
              }
            }
          } else {
            for (int i = 0; i < instruction->call_args.len; ++i) {
              IR_Value *arg = instruction->call_args[i];
              read_into_register(X64_Register::RAX, arg);
              //fprintf(out, "\tmov QWORD [rsp - %d], rax\n", 8 * (i + 3 + 1/*skip closure pointer*/));
              //REX.W + 89 /r 	MOV r/m64,r64
              add_1b(t, REX_W);
              add_1b(t, 0x89);
              add_1b(t, modrm(0b10, X64_Register::RAX, X64_Register::RSP/*Acts as SIB*/));
              add_1b(t, sib(0 /*scale*/, 0b100/*index none*/, 0b100 /*base [RSP]*/));
              add_4b(t, -8 * (i + 3 + 1/*skip closure pointer*/));
            }
          }

          if (call_directly_by_name) {
            assert(instruction->closure == NULL);
            //fprintf(out, "\tcall %s\n", callee->name);
            //E8 cd 	CALL rel32
            add_1b(t, 0xE8);
            auto offset = add_4b(t, 0); //relocation
            compiled_code.text_section_relocations.append({
              .symbol_name = callee->name,
              .offset = offset,
              .addend = -4
            });
          } else {
            if (instruction->closure) {
              read_into_register(X64_Register::RAX, instruction->closure);

              //fprintf(out, "\tmov QWORD [rsp - %d], rax\n", 8 * 3);
              //REX.W + 89 /r 	MOV r/m64,r64
              add_1b(t, REX_W);
              add_1b(t, 0x89);
              add_1b(t, modrm(0b01, X64_Register::RAX, X64_Register::RSP/*SIB*/));
              add_1b(t, sib(0, 0b100, 0b100));
              add_1b(t, -8*3);

            }
            read_into_register(X64_Register::RAX, instruction->op1);

            //FF /2 	CALL r/m64
            add_1b(t, 0xFF);
            add_1b(t, modrm(0b11, 2, X64_Register::RAX));
            //fprintf(out, "\tcall rax\n");
          }

          {
            // Saving callsite return addresses for nice stacktrace
            auto return_address_offset = t->len;
            assert(instruction->loc);
            auto code_location = decode_code_location(*instruction->loc);
            auto location_string = alloc_sprintf("%s:%d:%d %s", code_location.file_name, code_location.line, code_location.col, function->language_level_name);
            callsites.append({
              .function_symbol = symbol.name,
              .offset_in_function_code = (uint32_t)(return_address_offset - symbol.offset),
              .location = location_string,
            });
          }

          if (is_c_call && returns_float) {
            //fprintf(out, "\tmovq rax, xmm0\n");
            //66 REX.W 0F 7E /r MOVQ r/m64, xmm
            add_1b(t, 0x66);
            add_1b(t, REX_W);
            add_1b(t, 0x0F);
            add_1b(t, 0x7E);
            add_1b(t, modrm(0b11, X64_Register::XMM0, X64_Register::RAX));
          }
          write_from_register(instruction->res, X64_Register::RAX);
        } break;

        }
      }
    }

    FOR (jumps_to_patch) {
      const auto &jump_to_patch = *it;

      bool patched = false;
      FOR (block_offsets) {
        const auto &block_offset = *it;
        if (jump_to_patch.block == block_offset.block) {
          auto value = block_offset.start_offset - (jump_to_patch.offset_to_patch + 4);
          add_4b(t, value, jump_to_patch.offset_to_patch);
          patched = true;
          break;
        }
      }

      assert(patched);
    }

    symbol.length = compiled_code.text_section_buffer.len - symbol.offset;

    compiled_code.symbols.append(symbol);
  }

  {
    //Global symbol _start
    Compiled_Code::Symbol symbol = {};
    symbol.name = "_start";
    symbol.section = Compiled_Code::Section::TEXT;
    symbol.kind = Compiled_Code::Kind::FUNCTION;

    symbol.offset = compiled_code.text_section_buffer.len;

    //call entry_point
    // E8 cd
    add_1b(t, 0xE8);
    auto call_pc_offset_to_patch = add_4b(t, 0);

    auto next_instruction_pc = compiled_code.text_section_buffer.len;
    auto offset = compiled_code.entry_function_offset - next_instruction_pc;
    add_4b(t, offset, call_pc_offset_to_patch);

    //mov rdi, rax
    //REX.W 8B /r
    add_1b(t, 0x40 | (1 << 3)); // REX.W
    add_1b(t, 0x8B); // 8B
    add_1b(t, modrm(0b11, 0b111/*rdi*/, 0b0/*rax*/)); // ModRM

    //mov rax, 60
    //REX.W + B8+ rd io
    add_1b(t, 0x40 | (1 << 3)); // REX.W
    add_1b(t, 0xB8 + 0b0/*rax*/); // B8+rd
    add_8b(t, 60); // io = 60

    //syscall
    //0F 05
    add_1b(t, 0x0F);
    add_1b(t, 0x05);

    symbol.length = compiled_code.text_section_buffer.len - symbol.offset;

    compiled_code.symbols.append(symbol);
  }

  if (compiled_code.text_section_relocations.len) {
    for (int i = 0; i < compiled_code.text_section_relocations.len;) {
      auto relocation = compiled_code.text_section_relocations[i];
      if (relocation.is_absolute) {
        i++;
        continue;
      }

      bool relocated = false;
      FOR (compiled_code.symbols) {
        auto symbol = it;
        if (symbol->kind == Compiled_Code::Kind::FUNCTION && symbol->length && strcmp(symbol->name, relocation.symbol_name) == 0) {
          int32_t new_value = symbol->offset - relocation.offset + relocation.addend;
          add_4b(&compiled_code.text_section_buffer, new_value, relocation.offset);
          relocated = true;
        }
      }

      if (relocated) {
        compiled_code.text_section_relocations[i] = compiled_code.text_section_relocations.last();
        compiled_code.text_section_relocations.len--;
      } else {
        i++;
      }
    }
  }

  {
    auto callsites_strings_begin_offset = compiled_code.rodata_section_buffer.len;

    int offset = 0;
    FOR (callsites) {
      int len = 0;

      it->string_offset_in_array = offset;

      for (const char *char_it = it->location; *char_it; char_it++) {
        add_1b(&compiled_code.rodata_section_buffer, *char_it);
        offset++;
        len++;
      }

      it->string_len_in_array = len;
    }

    while (offset % 16 != 0) {
      add_1b(&compiled_code.rodata_section_buffer, 0);
      offset++;
    }

    auto callsites_strings_end_offset = compiled_code.rodata_section_buffer.len;

    {
      Compiled_Code::Symbol symbol = {};
      symbol.name = "__callsites_strings";
      symbol.section = Compiled_Code::Section::RODATA;
      symbol.kind = Compiled_Code::Kind::OBJECT;

      symbol.offset = callsites_strings_begin_offset;
      symbol.length = callsites_strings_end_offset - callsites_strings_begin_offset;
      compiled_code.symbols.append(symbol);
    }


    auto callsites_array_begin_offset = compiled_code.rodata_section_buffer.len;

    FOR (callsites) {
      auto relocation_offset = add_8b(&compiled_code.rodata_section_buffer, 0);
      add_4b(&compiled_code.rodata_section_buffer, it->string_offset_in_array);
      add_4b(&compiled_code.rodata_section_buffer, it->string_len_in_array);

      compiled_code.rodata_section_relocations.append({.symbol_name = it->function_symbol, .offset = relocation_offset, .addend = it->offset_in_function_code, .is_absolute = true});
    }

    auto callsites_array_end_offset = compiled_code.rodata_section_buffer.len;

    {
      Compiled_Code::Symbol symbol = {};
      symbol.name = "__callsites_array_begin";
      symbol.section = Compiled_Code::Section::RODATA;
      symbol.kind = Compiled_Code::Kind::OBJECT;

      symbol.offset = callsites_array_begin_offset;
      symbol.length = callsites_array_end_offset - callsites_array_begin_offset;
      compiled_code.symbols.append(symbol);
    }

    {
      Compiled_Code::Symbol symbol = {};
      symbol.name = "__callsites_array_end";
      symbol.section = Compiled_Code::Section::RODATA;
      symbol.kind = Compiled_Code::Kind::OBJECT;

      symbol.offset = callsites_array_end_offset;
      symbol.length = 0;
      compiled_code.symbols.append(symbol);
    }
  }

  //assert(compiled_code.text_section_relocations.len == 0);
  //assert(compiled_code.rodata_section_relocations.len == 0);

  const size_t symbol_table_entry_size = 24; /*according to elf.h*/
  Buffer symbol_table_buffer = {};
  Buffer *symbol_table = &symbol_table_buffer;
  reserve(symbol_table, 1024);
  zero_pad(symbol_table, symbol_table_entry_size); // first entry in symbol table should be zero
                                                   //
  Buffer string_table_buffer = {};
  Buffer *string_table = &string_table_buffer;
  reserve(string_table, 1024);
  add_1b(string_table, 0);

  FOR (compiled_code.symbols) {
    auto *symbol = it;

    const uint32_t name_start = add_bytes(string_table, (const uint8_t *)symbol->name, strlen(symbol->name) + 1);

    auto offset_before_symbol = symbol_table->len;

    // st_name;
    add_4b(symbol_table, name_start);

    // st_info
    if (symbol->kind == Compiled_Code::Kind::FUNCTION) {
      add_1b(symbol_table, (1 << 4/*global*/) | 2/*function*/); // STB_GLOBAL, STT_FUNCTION
    } else if (symbol->kind == Compiled_Code::Kind::OBJECT) {
      add_1b(symbol_table, (1 << 4/*global*/) | 1/*object*/); // STB_GLOBAL, STT_OBJECT
    } else if (symbol->kind == Compiled_Code::Kind::UNKNOWN) {
      add_1b(symbol_table, (1 << 4/*global*/) | 0/*unknown*/);
    } else {
      assert(0 && "unhandled symbol kind");
    }

    // st_other
    add_1b(symbol_table, 0); // default visibility

    // st_shndx
    if (symbol->section == Compiled_Code::Section::TEXT) {
      add_2b(symbol_table, text_section_number);
    } else if (symbol->section == Compiled_Code::Section::RODATA) {
      add_2b(symbol_table, rodata_section_number);
    } else if (symbol->section == Compiled_Code::Section::EXTERN) {
      add_2b(symbol_table, 0);
    } else {
      assert(0 && "unhandled symbol section");
    }

    // st_value
    add_8b(symbol_table, symbol->offset);

    // st_size
    add_8b(symbol_table, symbol->length);

    auto offset_after_symbol = symbol_table->len;
    assert((offset_after_symbol - offset_before_symbol) % symbol_table_entry_size == 0);
  }


  const size_t section_header_size = 64;
  const size_t rela_entry_size = 24;

  Buffer output_buffer = {};
  Buffer *b = &output_buffer;
  reserve(b, 64 * 1024);

  // ELF object file entry
  int32_t section_header_table_offset_in_elf_header = 0;
  int32_t section_header_table_count_offset_in_elf_header = 0;
  {
    // e_ident
    // Magic number
    add_1b(b, 0x7F);
    add_1b(b, 'E');
    add_1b(b, 'L');
    add_1b(b, 'F');

    // class
    add_1b(b, 2/*64 bit*/);

    // data encoding
    add_1b(b, 1/*little endian*/);

    // verion
    add_1b(b, 1/*current*/);

    // os_abi
    add_1b(b, 0/*system V*/);

    // os_abi_version
    add_1b(b, 0/*unspec?*/);


    for (int i = b->len; i < 16; i++) add_1b(b, 0);
    assert(b->len == 16);

    // e_type - type of elf file (object file, executable, SO, coredump etc)
    add_2b(b, 1/*Object file*/);

    // e_machine - architecture
    add_2b(b, 62/*EM_X86_64 	62 	AMD x86-64 architecture*/);

    // e_version - ELF file version
    add_4b(b, 1/*Current*/);

    // e_entry - address of to jump after program start
    add_8b(b, 0/*no entry point*/);

    // e_phoff; - program header table
    add_8b(b, 0/*no program header*/);

    // e_shoff; - section header table
    section_header_table_offset_in_elf_header = add_8b(b, 0/* will be patched later*/);

    // e_flags;
    add_4b(b, 0/* no flags */);

    // e_ehsize - size of ELF header
    add_2b(b, 64/* 64 bytes from elf.h */);

    // e_phentsize, e_phnum - size of single program header entry and count
    add_2b(b, 0);
    add_2b(b, 0);

    // e_shentsize, e_shnum - size of single section header entry and count
    add_2b(b, section_header_size/* from elf.h */);
    section_header_table_count_offset_in_elf_header = add_2b(b, 0/* will be patched */);

    // e_shstrndx - which section header is section name string table
    add_2b(b, 1);
    assert(b->len == 64);
  }

  assert(section_header_table_offset_in_elf_header);
  assert(section_header_table_count_offset_in_elf_header);

  //section headers table
  add_8b(b, b->len, section_header_table_offset_in_elf_header);

  uint32_t sections_count = 1;
  zero_pad(b, section_header_size); // section header table item at index 0
                                    // should be zero initialiazed

  uint32_t names_string_table_offset_to_patch = 0;
  uint32_t names_string_table_size_to_patch = 0;

  {
    // section for name string table

    uint32_t before_section = b->len;

    const char name[] = ".strtab";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 3 /*string table*/);

    // sh_flag
    add_8b(b, 0);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    names_string_table_offset_to_patch = add_8b(b, 0);

    // sh_size
    names_string_table_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, 0);

    // sh_info
    add_4b(b, 0);

    // sh_addralign
    add_8b(b, 0);

    // sh_entsize
    add_8b(b, 0);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }

  uint32_t symbol_table_offset_to_patch = 0;
  uint32_t symbol_table_size_to_patch = 0;
  {
    // section symbols
    assert(symtab_section_number == sections_count);

    uint32_t before_section = b->len;

    const char name[] = ".symtab";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 2 /*symbols table */);

    // sh_flag
    add_8b(b, 0);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    symbol_table_offset_to_patch = add_8b(b, 0);

    // sh_size
    symbol_table_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, 1/*link to string table*/);

    // sh_info
    //   One greater than the symbol table index of the last local symbol (binding STB_LOCAL).
    //   For now we will make all symbols global
    add_4b(b, 1);

    // sh_addralign
    add_8b(b, 0);

    // sh_entsize
    add_8b(b, symbol_table_entry_size);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }

  int32_t text_section_header_offset_to_patch = 0;
  int32_t text_section_header_size_to_patch = 0;
  {
    // section text
    assert(text_section_number == sections_count);

    uint32_t before_section = b->len;

    const char name[] = ".text";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 1 /*progbits*/);

    // sh_flag
    add_8b(b, 0x2/*SHF_ALLOC*/ | 0x4/*SHF_EXECINSTR*/);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    text_section_header_offset_to_patch = add_8b(b, 0);

    // sh_size
    text_section_header_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, 1/*link to string table*/);

    // sh_info
    add_4b(b, 0);

    // sh_addralign
    add_8b(b, 0);

    // sh_entsize
    add_8b(b, symbol_table_entry_size);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }

  int32_t rodata_section_header_offset_to_patch = 0;
  int32_t rodata_section_header_size_to_patch = 0;
  {
    assert(rodata_section_number == sections_count);

    uint32_t before_section = b->len;

    const char name[] = ".rodata";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 1 /*progbits*/);

    // sh_flag
    add_8b(b, 0x2/*SHF_ALLOC*/);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    rodata_section_header_offset_to_patch = add_8b(b, 0);

    // sh_size
    rodata_section_header_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, 1/*link to string table*/);

    // sh_info
    add_4b(b, 0);

    // sh_addralign
    add_8b(b, 16);

    // sh_entsize
    add_8b(b, 0);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }

  int32_t rela_text_section_header_offset_to_patch = 0;
  int32_t rela_text_section_header_size_to_patch = 0;
  {
    uint32_t before_section = b->len;

    const char name[] = ".rela.text";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 4 /*rela*/);

    // sh_flag
    add_8b(b, 0);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    rela_text_section_header_offset_to_patch = add_8b(b, 0);

    // sh_size
    rela_text_section_header_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, symtab_section_number);

    // sh_info
    add_4b(b, text_section_number);

    // sh_addralign
    add_8b(b, 8);

    // sh_entsize
    add_8b(b, rela_entry_size);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }
  Buffer rela_text_buffer = {};
  reserve(&rela_text_buffer, 1024);

  FOR (compiled_code.text_section_relocations) {
    auto relocation = *it;

    int64_t symbol_id = -1;
    for (int i = 0; i < compiled_code.symbols.len; ++i) {
      auto &symbol = compiled_code.symbols[i];
      if (strcmp(relocation.symbol_name, symbol.name) == 0) {
        symbol_id = i;
        break;
      }
    }
    assert(symbol_id != -1);

    symbol_id += 1;

    auto size_before = rela_text_buffer.len;
    // r_offset
    add_8b(&rela_text_buffer, relocation.offset);
    // r_info
    if (relocation.is_absolute) {
      add_8b(&rela_text_buffer, (symbol_id << 32) | (1 /*R_AMD64_64*/));
    } else {
      add_8b(&rela_text_buffer, (symbol_id << 32) | (2 /*R_AMD64_PC32*/));
    }
    // r_addend
    add_8b(&rela_text_buffer, relocation.addend);

    auto size_after = rela_text_buffer.len;
    assert(size_after - size_before == rela_entry_size);
  }

  int32_t rela_rodata_section_header_offset_to_patch = 0;
  int32_t rela_rodata_section_header_size_to_patch = 0;
  {
    uint32_t before_section = b->len;

    const char name[] = ".rela.rodata";
    auto name_offset_in_string_table = add_bytes(string_table, (const uint8_t *)name, sizeof(name));

    // sh_name - start index of zero terminated string in string table for section header names
    add_4b(b, name_offset_in_string_table);

    // sh_type
    add_4b(b, 4 /*rela*/);

    // sh_flag
    add_8b(b, 0);

    // sh_addr
    add_8b(b, 0);

    // sh_offset
    rela_rodata_section_header_offset_to_patch = add_8b(b, 0);

    // sh_size
    rela_rodata_section_header_size_to_patch = add_8b(b, 0);

    // sh_link
    add_4b(b, symtab_section_number);

    // sh_info
    add_4b(b, rodata_section_number);

    // sh_addralign
    add_8b(b, 8);

    // sh_entsize
    add_8b(b, rela_entry_size);

    uint32_t after_section = b->len;
    assert((after_section - before_section) % section_header_size == 0);

    sections_count += 1;
  }

  Buffer rela_rodata_buffer = {};
  reserve(&rela_rodata_buffer, 1024);

  FOR (compiled_code.rodata_section_relocations) {
    auto relocation = *it;

    int64_t symbol_id = -1;
    for (int i = 0; i < compiled_code.symbols.len; ++i) {
      auto &symbol = compiled_code.symbols[i];
      if (strcmp(relocation.symbol_name, symbol.name) == 0) {
        symbol_id = i;
        break;
      }
    }
    assert(symbol_id != -1);

    symbol_id += 1;

    auto size_before = rela_rodata_buffer.len;
    // r_offset
    add_8b(&rela_rodata_buffer, relocation.offset);
    // r_info
    if (relocation.is_absolute) {
      add_8b(&rela_rodata_buffer, (symbol_id << 32) | (1 /*R_AMD64_64*/));
    } else {
      add_8b(&rela_rodata_buffer, (symbol_id << 32) | (2 /*R_AMD64_PC32*/));
    }
    // r_addend
    add_8b(&rela_rodata_buffer, relocation.addend);

    auto size_after = rela_rodata_buffer.len;
    assert(size_after - size_before == rela_entry_size);
  }


  // patching number section number in ELF header
  add_2b(b, sections_count, section_header_table_count_offset_in_elf_header);

  // adding string table
  auto beginning_of_section_names = add_bytes(b, (const uint8_t *)string_table->data, string_table->len);

  // patching string table start and size in its section header
  add_8b(b, beginning_of_section_names, names_string_table_offset_to_patch);
  add_8b(b, string_table->len, names_string_table_size_to_patch);

  // writing symbol table
  auto beginning_of_symbols_table = add_bytes(b, (const uint8_t *)symbol_table->data, symbol_table->len);

  // writing text section
  auto beginning_of_text_section = add_bytes(b, (const uint8_t *) compiled_code.text_section_buffer.data, compiled_code.text_section_buffer.len);

  // writing rodata section
  auto beginning_of_rodata_section = add_bytes(b, (const uint8_t *) compiled_code.rodata_section_buffer.data, compiled_code.rodata_section_buffer.len);

  // writing rela.text section
  auto beginning_of_rela_text_section = add_bytes(b, (const uint8_t *) rela_text_buffer.data, rela_text_buffer.len);

  // writing rela.rodata section
  auto beginning_of_rela_rodata_section = add_bytes(b, (const uint8_t *) rela_rodata_buffer.data, rela_rodata_buffer.len);

  // patching symbol table section header
  add_8b(b, beginning_of_symbols_table, symbol_table_offset_to_patch);
  add_8b(b, symbol_table->len, symbol_table_size_to_patch);

  add_8b(b, beginning_of_text_section, text_section_header_offset_to_patch);
  add_8b(b, compiled_code.text_section_buffer.len, text_section_header_size_to_patch);

  add_8b(b, beginning_of_rodata_section, rodata_section_header_offset_to_patch);
  add_8b(b, compiled_code.rodata_section_buffer.len, rodata_section_header_size_to_patch);

  add_8b(b, beginning_of_rela_text_section, rela_text_section_header_offset_to_patch);
  add_8b(b, rela_text_buffer.len, rela_text_section_header_size_to_patch);

  add_8b(b, beginning_of_rela_rodata_section, rela_rodata_section_header_offset_to_patch);
  add_8b(b, rela_rodata_buffer.len, rela_rodata_section_header_size_to_patch);

  {
    auto file = fopen(alloc_sprintf("%s.o", to), "wb");
    if (!file) {
      report_error({}, "Failed to object file for writing");
      exit(1);
    }
    fwrite(output_buffer.data, 1, output_buffer.len, file);
    fclose(file);
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
  status = system(alloc_sprintf("ld -znoexecstack -x %s.o support_library.o -o %s", to, to));
  if (status) {
    report_error({}, "ld call returned %d\n", status);
    exit(1);
  }
}
