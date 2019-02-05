#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>
#include "vm.h"
#define MAX_INPUT_BUFFER_SIZE 65535
#define MAX_TOKEN_BUFFER_SIZE 128


int main(int argc, const char* argv[]) {
  if (argc != 2) {
    fputs("Usage: ./vm <META II bytecode file>\n", stdout);
    return 1;
  }
  FILE *file = 0;

  file = fopen(argv[1], "r");
  if (!file) {
    eprintf("Failed to open file %s.\n", argv[1]);
  }
  char input[MAX_INPUT_BUFFER_SIZE];
  uint8_t vm_code[MAX_INPUT_BUFFER_SIZE];

  const char * vm_input = "foo bar doge\n";

  char token[MAX_TOKEN_BUFFER_SIZE];
  uint16_t chars_read = fread(vm_code, sizeof(*vm_code), MAX_INPUT_BUFFER_SIZE, file);
  if (!chars_read) {
    eprintf("Couldn't read from file.");
  }
  fclose(file);
    
  mvm_t* main_vm = mvm_new(vm_code, vm_input, chars_read);
  
  mvm_run_N_instructions(main_vm, chars_read);
  
  mvm_free(main_vm);
  return 0;
  
  
}
