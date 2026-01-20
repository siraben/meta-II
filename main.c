#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "vm.h"

#define MAX_INPUT_BUFFER_SIZE 65535
#define MAX_TOKEN_BUFFER_SIZE 128
#define MAX_LINE_SIZE 1000

int main(int argc, const char* argv[]) {
  if (argc < 2) {
    fputs("Usage: ./vm <bytecode> [<source-file>] [-v]\n", stdout);
    fputs("  If source-file is provided, process entire file\n", stdout);
    fputs("  Otherwise, enter interactive REPL mode\n", stdout);
    fputs("  -v: Enable verbose/debug output\n", stdout);
    return 1;
  }

  // Check for verbose flag
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "-v") == 0) {
      debug = 1;
    }
  }

  FILE* file;
  file = fopen(argv[1], "r");

  if (!file) {
    eprintf("Failed to open bytecode file %s.\n", argv[1]);
  }
  char vm_input[MAX_INPUT_BUFFER_SIZE];
  uint8_t vm_code[MAX_INPUT_BUFFER_SIZE];
  size_t vm_bytes_read =
      fread(vm_code, sizeof(*vm_code), MAX_INPUT_BUFFER_SIZE, file);
  if (!vm_bytes_read) {
    eprintf("Couldn't read from bytecode file.");
  }
  fclose(file);

  mvm_t* main_vm;

  // Check if source file argument is provided (and is not the -v flag)
  if (argc >= 3 && strcmp(argv[2], "-v") != 0) {
    // File mode: read entire source file and process
    FILE* source_file = fopen(argv[2], "r");
    if (!source_file) {
      eprintf("Failed to open source file %s.\n", argv[2]);
    }

    size_t source_bytes_read = fread(vm_input, sizeof(char), MAX_INPUT_BUFFER_SIZE - 1, source_file);
    vm_input[source_bytes_read] = '\0';
    fclose(source_file);

    main_vm = mvm_new(vm_code, vm_input, vm_bytes_read);
    mvm_run_N_instructions(main_vm, 65535);
    mvm_free(main_vm);
  } else {
    // Interactive mode
    printf("Enter an expression to parse:\n");
    printf("> ");

    while (fgets(vm_input, MAX_LINE_SIZE, stdin)) {
      main_vm = mvm_new(vm_code, vm_input, vm_bytes_read);
      mvm_run_N_instructions(main_vm, 1000);
      mvm_free(main_vm);
      printf("> ");
    }
    putchar('\n');
  }

  return 0;
}
