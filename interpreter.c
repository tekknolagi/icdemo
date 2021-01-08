#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  // Load a constant from the constant array at index arg.
  CONST,
  // Add stack[-2] + stack[-1].
  ADD,
  // Print the top of the stack.
  PRINT,
  // Inspect (but do not pop) the top element on the stack. If less than 100,
  // set pc to arg.
  JUMP_IF_LESS_THAN_ONE_HUNDRED,
  HALT,
} Bytecode;

typedef enum {
  kInt,
  kStr,
} ObjectType;

typedef struct {
  ObjectType type;
  union {
    char *str_value;
    int int_value;
  };
} Object;

typedef unsigned char byte;

static unsigned kStackSize = 100;
static unsigned kBytecodeSize = 2;

typedef Object (*Method)(Object, Object);

Object add_ints(Object left, Object right) {
  Object result;
  result.type = kInt;
  result.int_value = left.int_value + right.int_value;
  return result;
}

Object add_strs(Object left, Object right) {
  Object result;
  result.type = kStr;
  result.str_value =
      malloc(strlen(left.str_value) + strlen(right.str_value) + 1);
  strcpy(result.str_value, left.str_value);
  strcat(result.str_value, right.str_value);
  return result;
}

Method lookup_method(ObjectType left_type, ObjectType right_type) {
  if (left_type == kInt && right_type == kInt) {
    return add_ints;
  }
  if (left_type == kStr && right_type == kStr) {
    return add_strs;
  }
  fprintf(stderr, "type error; cannot operate on str and int");
  abort();
}

void print_object(Object obj) {
  if (obj.type == kInt) {
    fprintf(stderr, "int: %d\n", obj.int_value);
  } else if (obj.type == kStr) {
    fprintf(stderr, "str: \"%s\"\n", obj.str_value);
  } else {
    fprintf(stderr, "<unknown>\n");
  }
}

void eval_code(byte *bytecode, int num_opcodes, Object *consts) {
  int pc = 0;
  Object *stack = calloc(kStackSize, sizeof *stack);
  Method *caches = calloc(num_opcodes, sizeof *caches);

#define PUSH(x) *stack++ = (x)
#define POP() *--stack
  while (true) {
    byte op = bytecode[pc];
    byte arg = bytecode[pc + 1];
    switch (op) {
    case CONST:
      PUSH(consts[arg]);
      break;
    case ADD: {
      Object right = POP();
      Object left = POP();
      Method cached = caches[pc >> 1];
      if (cached == NULL) {
        fprintf(stderr, "updating cache at %d\n", pc);
        cached = caches[pc >> 1] = lookup_method(left.type, right.type);
      } else {
        fprintf(stderr, "have cached value at %d\n", pc);
      }
      Object result = (*cached)(left, right);
      PUSH(result);
      break;
    }
    case PRINT: {
      Object obj = POP();
      print_object(obj);
      break;
    }
    case JUMP_IF_LESS_THAN_ONE_HUNDRED: {
      Object obj = *(stack - 1);
      if (obj.type != kInt) {
        abort();
      }
      if (obj.int_value < 100) {
        pc = arg;
        continue;
      }
      break;
    }
    case HALT:
      return;
    default:
      fprintf(stderr, "unknown opcode %d\n", op);
      abort();
    }
    pc += kBytecodeSize;
  }
}

int main() {
  byte bytecode[] = {/*0:*/ CONST,
                     0,
                     /*2:*/ CONST,
                     1,
                     /*4:*/ ADD,
                     0,
                     /*6:*/ JUMP_IF_LESS_THAN_ONE_HUNDRED,
                     2,
                     /*8:*/ PRINT,
                     0,
                     /*10:*/ HALT,
                     0};
  Object consts[4] = {
      (Object){.type = kInt, .int_value = 5},
      (Object){.type = kInt, .int_value = 10},
      (Object){.type = kStr, .str_value = "hello "},
      (Object){.type = kStr, .str_value = "world"},
  };
  eval_code(bytecode, sizeof bytecode / 2, consts);
}
