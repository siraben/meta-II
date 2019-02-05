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

#define DEBUG 1
void debug_print(const char *fmt, ...) {
  if (DEBUG) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stdout, fmt, ap);
    va_end(ap);
    fputc('\n', stdout); 
  }
}


void eprintf(const char *fmt, ...) {
va_list ap;

va_start(ap, fmt);
vfprintf(stderr, fmt, ap);
va_end(ap);
fputc('\n', stderr);
exit(1);
}

void clear_token_buffer(mvm_t *vm) {
  if (!vm) {
    eprintf("CLEAR_TOKEN_BUFFER: Null pointer received.");
  }
  memset(vm->token_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
}

void clear_scratch_buffer(mvm_t *vm) {
  if (!vm) {
    eprintf("CLEAR_SCRATCH_BUFFER: Null pointer received.");
  }
  memset(vm->scratch_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
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

uint8_t next_byte(mvm_t * vm) {
  if (!vm) {
    eprintf("NEXT_BYTE: Null pointer received.");
  }  
  vm->ip += 1;
  if (vm->ip > vm->code_size) {
    eprintf("NEXT_BYTE: Out of bounds.");
  }    
  return (vm->code[vm->ip]);
}


uint8_t current_byte(mvm_t * vm) {
  if (!vm) {
    eprintf("CURRENT_BYTE: Null pointer received.");
  }
  return vm->code[vm->ip];
}

void vm_tst_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_TST_INST: Null pointer received.");
  }  
  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;

  strncpy(vm->scratch_buffer, (char*)(vm->code + vm->ip), MAX_TOKEN_BUFFER_SIZE);
  strncpy(vm->token_buffer, vm->scratch_buffer, MAX_TOKEN_BUFFER_SIZE);
  uint16_t token_size = strlen(vm->token_buffer);  
  if (match(s, vm->scratch_buffer)) {
    debug_print("TST: Matched '%s', length %hu", vm->scratch_buffer, token_size);
    debug_print("TST: Advancing by %d to %d", token_size + 1, token_size + vm->ip + 1);
    mvm_set_switch(vm);
    
    mvm_advance_input_N(vm, token_size);
    // Also skip NULL delimiter.
    mvm_advance_ip_N(vm, token_size + 1);
    return;
  }
  mvm_reset_switch(vm);
  mvm_advance_ip_N(vm, token_size + 1);
  debug_print("TST: Failed to match '%s'", vm->scratch_buffer);
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

void vm_cl_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_CL_INST: Null pointer received.");
  }
  clear_token_buffer(vm);
  clear_scratch_buffer(vm);
  strncpy(vm->scratch_buffer, vm->code + vm->ip, MAX_TOKEN_BUFFER_SIZE);
  size_t msglength = strlen(vm->scratch_buffer);
  debug_print("CL: Printing '%s', length %d", vm->scratch_buffer, msglength);
  printf("%s", vm->scratch_buffer);
  mvm_advance_ip_N(vm, msglength + 1);
  
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
bool is_delimiter(char c) {
  return c == ' ' || c == '\n' || c == '\t';
}

void vm_id_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_ID_INST: Null pointer received.");
  }

  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *end;
  skip_whitespace(s);
  if (!isalpha(**s) && !isnumber(**s)) {
    debug_print("ID failed");
    return;
  }  
  end = strpbrk(*s, delims);
  if (end) {
    size_t id_size = strcspn(*s, delims);
    memset(vm->token_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
    strncpy(vm->token_buffer, *s, id_size);
    mvm_advance_input_N(vm, id_size + 1);
    mvm_set_switch(vm);
    debug_print("ID: %s\nSize: %lu", vm->token_buffer, id_size);
    return;
  }

  mvm_reset_switch(vm);

  debug_print("ID failed");
  return;
  
}

void vm_num_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_NUM_INST: Null pointer received.");
  }
  debug_print("NUM: Entered");
  char *original = vm->input_buffer_pointer + vm->input_buffer;
  char **s = &original;
  char *end;
  skip_whitespace(s);
  if (!isnumber(**s)) {
    debug_print("NUM: Failed");
    mvm_reset_switch(vm);
    return;
  }

  // Otherwise we are at the beginning of a string.
  end = strpbrk(*s, delims);
  if (end) {
    size_t id_size = strspn(*s, "1234567890");
    memset(vm->token_buffer, 0, MAX_TOKEN_BUFFER_SIZE);
    
    strncpy(vm->token_buffer, *s, id_size);
    mvm_advance_input_N(vm, id_size);
    mvm_set_switch(vm);
    debug_print("NUM: %s\nSize: %lu", vm->token_buffer, id_size);
    return;
  }

  mvm_reset_switch(vm);

  debug_print("NUM: Failed");
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
    debug_print("SR failed");
    mvm_reset_switch(vm);
    return;
  }
  (*s)++;
  // Otherwise we are at the beginning of a string.
  end = strpbrk(*s, "'");
  if (end) {
    size_t id_size = strcspn(*s, "'");
    id_size++;
    (*s)--;
    

    clear_token_buffer(vm);
    // Copy the string quotes too.
    strncpy(vm->token_buffer, *s, id_size + 1);
    mvm_advance_input_N(vm, id_size + 1);
    mvm_set_switch(vm);
    debug_print("SR: %s\nSize: %lu", vm->token_buffer, id_size);
    return;
  }

  mvm_reset_switch(vm);

  debug_print("SR failed");
  return;
  
}

void vm_end_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_END_INST: Null pointer received.");
  }  
  printf("Ending machine.\n");
  vm->ip = vm->code_size - 1;
  return;
}

void vm_b_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_B_INST: Null pointer received.");
  }  
  uint16_t dest;
  uint8_t low = current_byte(vm);
  uint8_t high = next_byte(vm);  
  dest = low + (high << 8);
  debug_print("B: Branching to %d", dest);

  vm->ip = dest;
  if (vm->ip > vm->code_size) {
    eprintf("VM_B_INST: Out of bounds.");
  }

}

void vm_be_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_BE_INST: Null pointer received.");
  }
  if (!(vm->the_switch)) {
    eprintf("VM_BE_INST: BE instruction encountered, stopping.");
  }
}

void vm_bt_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_BT_INST: Null pointer received.");
  }  
  uint16_t dest;
  uint8_t low = current_byte(vm);
  uint8_t high = next_byte(vm);  
  dest = low + (high << 8);
  if (dest >= vm->code_size) {
    eprintf("VM_BT_INST: Out of bounds.");
  }

  next_byte(vm);
  
  if (vm->the_switch) {
    debug_print("BT: Branching to %d", dest);
    vm->ip = dest;
  }
  
}

void vm_bf_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_BF_INST: Null pointer received.");
  }  
  uint16_t dest;
  uint8_t low = current_byte(vm);
  uint8_t high = next_byte(vm);  
  dest = low + (high << 8);
  if (dest >= vm->code_size) {
    eprintf("VM_BF_INST: Out of bounds.");
  }

  next_byte(vm);
  if (!vm->the_switch) {
    debug_print("BF: Branching to %d", dest);
    vm->ip = dest;
  }
  
}

void vm_cll_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_CLL_INST: Null pointer received.");
  }
  uint16_t call_dest;
  uint8_t low = current_byte(vm);
  uint8_t high = next_byte(vm);  
  call_dest = low + (high << 8);
  if (call_dest >= vm->code_size) {
    eprintf("VM_CLL_INST: Out of bounds.");
  }
  uint8_t sp = vm->stack_pointer;
  debug_print("VM_CLL: Stack at %d", sp);
  debug_print("VM_CLL: Calling %d", call_dest);  
  if (sp >= 255) {
    eprintf("VM_CLL_INST: Stack overflow.");
  }
  vm->stack[sp].label1 = 0;
  vm->stack[sp].label2 = 0;
  vm->stack[sp].return_address = vm->ip + 1;
  vm->stack_pointer++;
  vm->stack_empty = false;
    
  vm->ip = call_dest;
  
}

void vm_r_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_R_INST: Null pointer received.");
  }
  
  uint8_t sp = vm->stack_pointer;
  uint16_t return_dest = vm->stack[sp].return_address;
  debug_print("R: Returning to %d", return_dest);
  if (vm->stack_empty) {
    eprintf("VM_R_INST: Stack underflow.");
  }
  

  if (return_dest >= vm->code_size) {
    eprintf("VM_R_INST: Out of bounds.");
  }
  if (sp > 0) {
    vm->stack_pointer--;
  } else {
    vm->stack_empty = true;
  }
  
  vm->ip = return_dest;
  
}

void vm_gn1_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_GN1_INST: Null pointer received.");
  }
  uint8_t sp = vm->stack_pointer;
  vm->stack[sp].label1++;
  printf("A%d\n", vm->stack[sp].label1);
}


void vm_gn2_inst(mvm_t *vm) {
  if (!vm) {
    eprintf("VM_GN2_INST: Null pointer received.");
  }
  uint8_t sp = vm->stack_pointer;
  vm->stack[sp].label2++;
  printf("B%d\n", vm->stack[sp].label2);  
}

void install_opcodes(mvm_t *vm) {
  vm->opcodes[TST] = vm_tst_inst;
  vm->opcodes[ID]  = vm_id_inst;
  vm->opcodes[NUM] = vm_num_inst;
  vm->opcodes[SR]  = vm_sr_inst;
  vm->opcodes[CLL] = vm_cll_inst;
  vm->opcodes[R]   = vm_r_inst;
  vm->opcodes[SET] = vm_set_inst;
  vm->opcodes[B]   = vm_b_inst;
  vm->opcodes[BT]  = vm_bt_inst;
  vm->opcodes[BF]  = vm_bf_inst;
  vm->opcodes[BE]  = vm_be_inst;
  vm->opcodes[CL]  = vm_cl_inst;
  vm->opcodes[CI]  = vm_ci_inst;
  vm->opcodes[GN1] = vm_gn1_inst;
  vm->opcodes[GN2] = vm_gn2_inst;
  vm->opcodes[LB]  = vm_lb_inst;
  vm->opcodes[OUT] = vm_out_inst;
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
  res->stack_empty = true;
  strncpy(res->input_buffer, input, strlen(input));
  return res;
  
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
    current_instruction = vm->opcodes[vm->code[vm->ip]];
    if (!current_instruction) {
      eprintf("Invalid instruction: %d", vm->code[vm->ip]);
    }
    // Advance IP before executing
    debug_print("Cycle: %d\nInstruction: %d", i, vm->code[vm->ip]);
    if (DEBUG) { mvm_print_info(vm); }        
    next_byte(vm);
    current_instruction(vm);
    if (vm->input_buffer_pointer == vm->input_size ||
        (1 + vm->ip) >= vm->code_size) {
      printf("VM execution done.\n");
      return;
    }
    
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
  // printf("Code length: %d\n", vm->code_size);
  // printf("Input length: %d\n", vm->input_size);
  printf("Instruction pointer: %hu\n", vm->ip);
  printf("Rest of input: >%s", vm->input_buffer + vm->input_buffer_pointer);
  printf("Input buffer pointer: %hu\n", vm->input_buffer_pointer);
  // printf("Output buffer pointer: %hu\n", vm->output_buffer_pointer);
  printf("Stack pointer: %d and %sempty\n", vm->stack_pointer, vm->stack_empty ? "" : "not ");
  printf("Switch: %d\n", vm->the_switch);

  
  puts("");
  return;
}
