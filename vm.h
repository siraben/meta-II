
#ifndef VM_H_
#define VM_H_

#include <stdbool.h>
#include <stdint.h>

#define MAX_INPUT_BUFFER_SIZE 65535
#define MAX_TOKEN_BUFFER_SIZE 128
#define MAX_CODE_BYTES 65535

int debug;
typedef struct mvm_stackframe {
  uint8_t label1;
  uint8_t label2;
  uint16_t return_address;
} mvm_stackframe_t;

struct mvm;
// The type of an opcode.
typedef void opcode_implementation(struct mvm *in);

typedef struct mvm {
  bool stack_empty;
  bool the_switch;
  uint16_t ip;
  uint8_t code[MAX_CODE_BYTES];
  uint16_t code_size;
  uint16_t input_size;  
  
  opcode_implementation *opcodes[256];
  mvm_stackframe_t stack[256];

  // We store pointers like this so that the VM can be self-contained.
  // The stack pointer is pointing at the next free stack entry.
  uint8_t stack_pointer;
  
  char input_buffer[MAX_INPUT_BUFFER_SIZE];
  uint16_t input_buffer_pointer;
  
  uint16_t output_buffer_pointer;
  char output_buffer[MAX_INPUT_BUFFER_SIZE];
  
  char token_buffer[MAX_TOKEN_BUFFER_SIZE];
  char scratch_buffer[MAX_TOKEN_BUFFER_SIZE];

  uint8_t col;
} mvm_t;


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


// Allocate memory for a new virtual machine.
mvm_t *mvm_new(uint8_t *code, const char *input, uint16_t size);

// Set error handler.
void mvm_set_error_handler(mvm_t *vm, void (*fp) (char *msg));

void mvm_print_info(mvm_t *vm);

void mvm_free(mvm_t *vm);

void mvm_run(mvm_t *vm);

// Run a VM but a limited number of instructions.
void mvm_run_N_instructions(mvm_t *vm, uint16_t max_instructions);

void eprintf(const char *, ...);
char *estrndup(const char *, size_t);
void mvm_advance_ip_N(mvm_t * vm, uint16_t amount);
void mvm_advance_input_N(mvm_t * vm, uint16_t amount);


#endif
