#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "vm.h"
#define MAX_CODE_BYTES 65535

typedef enum {
  TST,
  ID,
  NUM,
  SR,
  CLL,
  R,
  SET,
  B,
  BT,
  BF,
  BE,
  CL,
  CI,
  GN1,
  GN2,
  LB,
  OUT,
  END
} meta_II_instruction;



void eprintf(const char *fmt, ...) {
  va_list ap;

  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  va_end(ap);
  fputc('\n', stderr);
  exit(1);
}

static bool match(char **s, char *token) {
  if (strncmp(*s, token, strlen(token)) == 0) {
    *s += strlen(token);
    return true;
  }

  return false;
}

static void skip_whitespace(char **s) {
  while (**s == ' ' || **s == '\n' || **s == '\t')
    (*s)++;
}


void mvm_advance_ip_N(mvm_t * vm, uint16_t amount);
void mvm_advance_input_N(mvm_t * vm, uint16_t amount);
  
void vm_test_inst(mvm_t *vm) {
  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *token = vm->ip + vm->token_buffer;
  // Need + 1 because IP is still on the instruction TST.`
  strncpy(vm->scratch, (char*)(vm->code + vm->ip + 1), MAX_TOKEN_BUFFER_SIZE);
  printf("TST '%s' -> ", vm->scratch);
  if (match(s, vm->scratch)) {
    strncpy(vm->token_buffer, vm->scratch, MAX_TOKEN_BUFFER_SIZE);
    uint16_t token_size = strlen(vm->token_buffer);
    printf("matched length %hu\n", token_size);
    // Also skip NULL delimiter.
    mvm_advance_input_N(vm, token_size + 1);
    mvm_advance_ip_N(vm, token_size + 1);
    return;
  }
  printf("failed\n");
  *s = original;
  return;

}

void vm_ci_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_CI_INST: Null pointer received.");
  }
  char *msg = vm->token_buffer;
  uint8_t col = vm->curr_col;
  if (col > 70) {
    eprintf("VM_CI_INST: Invalid column: %hu", col);
  }
  for(uint8_t i = 0; i < col; i++) {
    putchar(' ');
  }
  printf("%s\n", msg);
  return;
}
void install_opcodes(mvm_t *vm) {
  vm->opcodes[TST] = vm_test_inst;
  vm->opcodes[CI] = vm_ci_inst;
  return;
}
mvm_t *mvm_new(uint8_t *code, const char *input, uint16_t code_size) {
  mvm_t *res;
  res = calloc(1, sizeof(mvm_t));
  if (!res) {
    eprintf("MVM_NEW: Out of memory.");
  }
  memcpy(res->code, code, MAX_CODE_BYTES);
  res->code_size = code_size;
  // Install opcodes.
  install_opcodes(res);
  res->input_size = strlen(input);
  strncpy(res->input_buffer, input, strlen(input));
  return res;
  
}
unsigned char next_byte(mvm_t * vm) {
    vm->ip += 1;

    // Wrap around.
    if (vm->ip >= 0xFFFF){
        vm->ip = 0;
    }
    
    return (vm->code[vm->ip]);
}


void mvm_advance_ip_N(mvm_t * vm, uint16_t amount) {
    vm->ip += amount;
    if (vm->ip > vm->code_size) {
      eprintf("MVM_ADVANCE_IP_N: Out of bounds.");
    }
    // Wrap around.
    if (vm->ip >= 0xFFFF){
        vm->ip = 0;
    }
    return;
}


void mvm_advance_input_N(mvm_t * vm, uint16_t amount) {
    vm->input_buffer_pointer += amount;
    if (vm->input_buffer_pointer > vm->input_size) {
      eprintf("MVM_ADVANCE_INPUT_N: Out of bounds.");
    }
    // Wrap around.
    if (vm->input_buffer_pointer >= 0xFFFF) {
        vm->input_buffer_pointer = 0;
    }
    return;
}

void mvm_run_N_instructions(mvm_t *vm, uint16_t max_instructions) {
  printf("Initial start.\n");
  mvm_print_info(vm);
  opcode_implementation *current_instruction;
  for (uint16_t i = 0; i < max_instructions; i++) {
    printf("Cycle: %d\nInstruction: %d\n", i, vm->code[vm->ip]);
    current_instruction = vm->opcodes[vm->code[vm->ip]];
    if (!current_instruction) {
      eprintf("Invalid instruction: %d", vm->code[vm->ip]);
    }
    current_instruction(vm);
    mvm_print_info(vm);
    if (vm->input_buffer_pointer == vm->input_size) {
      printf("VM execution done.\n");
      return;
    }
    next_byte(vm);
  }
}

void mvm_free(mvm_t *vm) {
  if (!vm) {
    eprintf("MVM_FREE: Null pointer received.");
  }
  // Free the VM itself.
  free(vm);
}

void mvm_print_info(mvm_t *vm) {
  if (!vm) {
    eprintf("MVM_PRINT_INFO: Null pointer received.");
  }
  printf("Code length: %d\n", vm->code_size);
  printf("Input length: %d\n", vm->input_size);
  printf("Instruction pointer: %hu\n", vm->ip);
  printf("Input buffer pointer: %hu\n", vm->input_buffer_pointer);
  printf("Output buffer pointer: %hu\n", vm->output_buffer_pointer);
  printf("Stack pointer: %d\n", vm->stack_pointer);

  
  puts("");
  return;
}
