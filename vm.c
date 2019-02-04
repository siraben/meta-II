#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <stdbool.h>
#include <string.h>
#include <errno.h>
#include <ctype.h>

#define INPUT_BUFFER_SIZE 65535
#define TOKEN_BUFFER_SIZE 128


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
  
static void eprintf(const char *, ...);
static char *estrndup(const char *, size_t);

static void eprintf(const char *fmt, ...) {
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
  char input[INPUT_BUFFER_SIZE];
  char token[TOKEN_BUFFER_SIZE];
  char *copy;
  size_t chars_read = fread(input, sizeof(*input), INPUT_BUFFER_SIZE, file);
  printf("%ld\n", chars_read);
  
  fclose(file);
  return 0;
  
  
}
