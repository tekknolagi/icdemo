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
  HALT,
} Bytecode;

typedef enum {
  kInt,
  kStr,
  kError = -1,
} ObjectType;

typedef struct {
  ObjectType type;
  union {
    char *str_value;
    int int_value;
  };
} Object;

typedef enum {
  kAdd,
  kUnknownSymbol = -1,
} Symbol;

typedef Object (*Method)(Object, Object);

typedef struct {
  Symbol name;
  Method method;
} MethodDefinition;

typedef unsigned char byte;

static unsigned kBytecodeSize = 2;

typedef struct {
  byte *bytecode;
  int num_opcodes;
  Object *consts;
  Method *caches;
} Code;

Object add_ints(Object left, Object right) {
  if (left.type != kInt || right.type != kInt) {
    fprintf(stderr, "ERROR: expected int\n");
    return (Object){.type = kError};
  }
  Object result;
  result.type = kInt;
  result.int_value = left.int_value + right.int_value;
  return result;
}

Object add_strs(Object left, Object right) {
  if (left.type != kStr || right.type != kStr) {
    fprintf(stderr, "ERROR: expected str\n");
    return (Object){.type = kError};
  }
  Object result;
  result.type = kStr;
  result.str_value =
      malloc(strlen(left.str_value) + strlen(right.str_value) + 1);
  strcpy(result.str_value, left.str_value);
  strcat(result.str_value, right.str_value);
  return result;
}

static const MethodDefinition kIntMethods[] = {
    {kAdd, add_ints},
    {kError, NULL},
};

static const MethodDefinition kStrMethods[] = {
    {kAdd, add_strs},
    {kError, NULL},
};

static const MethodDefinition *kTypes[] = {
    [kInt] = kIntMethods,
    [kStr] = kStrMethods,
};

Method lookup_method(ObjectType left_type, Symbol name) {
  const MethodDefinition *table = kTypes[left_type];
  for (int i = 0; table[i].method != NULL; i++) {
    if (table[i].name == name) {
      return table[i].method;
    }
  }
  return NULL;
}

void print_object(Object obj) {
  if (obj.type == kInt) {
    fprintf(stderr, "int: %d\n", obj.int_value);
  } else if (obj.type == kStr) {
    fprintf(stderr, "str: \"%s\"\n", obj.str_value);
  } else if (obj.type == kError) {
    fprintf(stderr, "error\n");
  } else {
    fprintf(stderr, "<unknown>\n");
  }
}

Code new_code(byte *bytecode, int num_opcodes, Object *consts) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.consts = consts;
  result.caches = calloc(num_opcodes, sizeof *result.caches);
  return result;
}

void eval_code(Code *code) {
  int pc = 0;
#define STACK_SIZE 100
  Object stack_array[STACK_SIZE];
  Object *stack = stack_array;

#define PUSH(x) *stack++ = (x)
#define POP() *--stack
  while (true) {
    byte op = code->bytecode[pc];
    byte arg = code->bytecode[pc + 1];
    switch (op) {
    case CONST:
      PUSH(code->consts[arg]);
      break;
    case ADD: {
      Object right = POP();
      Object left = POP();
      // TODO(max): Use left hand side type as cache key; check and relookup if
      // mismatch
      Method cached = code->caches[pc / kBytecodeSize];
      if (cached == NULL) {
        fprintf(stderr, "updating cache at %d\n", pc);
        cached = code->caches[pc / kBytecodeSize] =
            lookup_method(left.type, kAdd);
      } else {
        fprintf(stderr, "have cached value at %d\n", pc);
      }
      Object result = (*cached)(left, right);
      if (result.type == kError) {
        abort();
      }
      PUSH(result);
      break;
    }
    case PRINT: {
      Object obj = POP();
      print_object(obj);
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
  byte bytecode[] = {/*0:*/ CONST, 0,
                     /*2:*/ CONST, 1,
                     /*4:*/ ADD,   0,
                     /*8:*/ PRINT, 0,
                     /*10:*/ HALT, 0};
  Object consts[4] = {
      (Object){.type = kInt, .int_value = 5},
      (Object){.type = kInt, .int_value = 10},
      (Object){.type = kStr, .str_value = "hello "},
      (Object){.type = kStr, .str_value = "world"},
  };
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize, consts);
  eval_code(&code);
  eval_code(&code);
  eval_code(&code);
  free(code.caches);
}
