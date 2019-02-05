#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#include "vm.h"
#define MAX_CODE_BYTES 65535
#define MAX_TOKEN_BUFFER_SIZE 128


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

void mvm_set_switch(mvm_t *vm) {
  if (!vm) {
    eprintf("MVM_SET_SWITCH: Null pointer received.");
  }
  vm->the_switch = 1;
  return;
}

void mvm_reset_switch(mvm_t *vm) {
  if (!vm) {
    eprintf("MVM_RESET_SWITCH: Null pointer received.");
  }
  vm->the_switch = 0;
  return;
}

void vm_tst_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_TST_INST: Null pointer received.");
  }  
  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *token = vm->ip + vm->token_buffer;
  // Need + 1 because IP is still on the instruction TST.
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
  return;

}

void vm_ci_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_CI_INST: Null pointer received.");
  }
  char *msg = vm->token_buffer;
  uint8_t col = vm->col;
  if (col > 70) {
    eprintf("VM_CI_INST: Invalid column: %hu", col);
  }
  for(uint8_t i = 0; i < col; i++) {
    putchar(' ');
  }
  printf("%s", msg);
  return;
}

void vm_set_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_SET_INST: Null pointer received.");
  }
  vm->the_switch = 1;
  return;
}

void vm_lb_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_LB_INST: Null pointer received.");
  }
  vm->col = 0;
  return;
}

void vm_out_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_OUT_INST: Null pointer received.");
  }
  printf("\n");
  vm->col = 7;
  return;
}

const char* delims = " \n\t";  // Set of symbol delimiter characters

void vm_id_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_ID_INST: Null pointer received.");
  }

  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *end;
  skip_whitespace(s);
  if (!isalpha(**s) && !isnumber(**s)) {
    printf("ID failed\n");
    return;
  }  
  end = strpbrk(*s, delims);
  if (end) {
    size_t id_size = strcspn(*s, delims);
    memset(vm->token_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
    strncpy(vm->token_buffer, *s, id_size);
    mvm_advance_input_N(vm, id_size + 1);
    mvm_set_switch(vm);
    printf("ID: %s\nSize: %lu\n", vm->token_buffer, id_size);
    return;
  }

  mvm_reset_switch(vm);

  printf("ID failed\n");
  return;
  
}

void vm_sr_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_SR_INST: Null pointer received.");
  }

  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *end;
  skip_whitespace(s);
  if (**s != '\'') {
    printf("SR failed\n");
    return;
  }
  (*s)++;
  // Otherwise we are at the beginning of a string.
  end = strpbrk(*s, "'");
  if (end) {
    size_t id_size = strcspn(*s, "'");
    id_size++;
    (*s)--;
    
    memset(vm->token_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
    // Copy the string quotes too.
    strncpy(vm->token_buffer, *s, id_size + 1);
    mvm_advance_input_N(vm, id_size + 1);
    mvm_set_switch(vm);
    printf("SR: %s\nSize: %lu\n", vm->token_buffer, id_size);
    return;
  }

  mvm_reset_switch(vm);

  printf("SR failed\n");
  return;
  
}

void vm_end_inst(mvm_t *vm) {
  printf("Ending machine.\n");
  vm->ip = vm->code_size - 1;
  return;
}

void install_opcodes(mvm_t *vm) {
  vm->opcodes[TST] = vm_tst_inst;
  vm->opcodes[CI] = vm_ci_inst;
  vm->opcodes[SET] = vm_set_inst;
  vm->opcodes[LB] = vm_lb_inst;
  vm->opcodes[ID] = vm_id_inst;
  vm->opcodes[OUT] = vm_out_inst;
  vm->opcodes[SR] = vm_sr_inst;
  vm->opcodes[END] = vm_end_inst;  
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
  if (vm->ip > vm->code_size) {
    eprintf("NEXT_BYTE: Out of bounds.");
  }    
  return (vm->code[vm->ip]);
}


void mvm_advance_ip_N(mvm_t * vm, uint16_t amount) {
  vm->ip += amount;
  if (vm->ip > vm->code_size) {
    eprintf("MVM_ADVANCE_IP_N: Out of bounds.");
  }

  return;
}

void mvm_advance_input_N(mvm_t * vm, uint16_t amount) {
  vm->input_buffer_pointer += amount;
  if (vm->input_buffer_pointer > vm->input_size) {
    eprintf("MVM_ADVANCE_INPUT_N: Out of bounds.");
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
    if (vm->input_buffer_pointer == vm->input_size ||
        (1 + vm->ip) >= vm->code_size) {
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
