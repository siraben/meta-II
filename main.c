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
#define MAX_LINE_SIZE 1000

int main(int argc, const char* argv[]) {
  if (argc != 2) {
    fputs("Usage: ./vm <META II bytecode file>\n", stdout);
    return 1;
  }
  FILE *file;

  file = fopen(argv[1], "r");
  
  if (!file) {
    eprintf("Failed to open file %s.\n", argv[1]);
  }
  char vm_input[MAX_INPUT_BUFFER_SIZE];
  uint8_t vm_code[MAX_INPUT_BUFFER_SIZE];
  size_t vm_bytes_read =  fread(vm_code, sizeof(*vm_code), MAX_INPUT_BUFFER_SIZE, file);
  if (!vm_bytes_read) {
    eprintf("Couldn't read from file.");
  }
  fclose(file);
  
  
  
  printf("Enter an expression to parse:\n");
  printf("> ");
  mvm_t* main_vm;
    
  while (fgets(vm_input, MAX_LINE_SIZE, stdin)) {

    main_vm = mvm_new(vm_code, vm_input, vm_bytes_read);
  
    mvm_run_N_instructions(main_vm, 1000);

    mvm_free(main_vm);
  
    printf("> ");    
  }
  
  putchar('\n');

    

  return 0;
  
  
}
