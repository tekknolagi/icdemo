#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  // Load a constant from the constant array at index `arg'.
  CONST,
  // Load a value from the arguments array at index `arg'.
  ARG,
  // Add stack[-2] + stack[-1].
  ADD,
  // Print the top of the stack.
  PRINT,
  HALT,
} Bytecode;

static unsigned kBytecodeSize = 2;

typedef enum {
  kInt,
  kStr,
  kError = -1,
} ObjectType;

typedef struct {
  ObjectType type;
  union {
    const char *str_value;
    int int_value;
  };
} Object;

Object new_int(int value) { return (Object){.type = kInt, .int_value = value}; }

Object new_str(const char *value) {
  return (Object){.type = kStr, .str_value = value};
}

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

typedef struct {
  ObjectType key;
  Method value;
} CachedValue;

typedef struct {
  byte *bytecode;
  int num_opcodes;
  Object *consts;
  // Array of `num_opcodes' elements.
  CachedValue *caches;
} Code;

Object add_ints(Object left, Object right) {
  if (left.type != kInt || right.type != kInt) {
    fprintf(stderr, "ERROR: expected int\n");
    return (Object){.type = kError};
  }
  return new_int(left.int_value + right.int_value);
}

Object add_strs(Object left, Object right) {
  if (left.type != kStr || right.type != kStr) {
    fprintf(stderr, "ERROR: expected str\n");
    return (Object){.type = kError};
  }
  int result_size = strlen(left.str_value) + strlen(right.str_value) + 1;
  char *result = malloc(result_size);
  strcpy(result, left.str_value);
  strcat(result, right.str_value);
  return new_str(result);
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
  fprintf(stderr, "could not find method\n");
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

void eval_code(Code *code, Object *args, int nargs) {
  int pc = 0;
#define STACK_SIZE 100
  Object stack_array[STACK_SIZE];
  Object *stack = stack_array;
#define PUSH(x) *stack++ = (x)
#define POP() *--stack
#define CACHE_AT(pc) code->caches[(pc) / kBytecodeSize]
  while (true) {
    byte op = code->bytecode[pc];
    byte arg = code->bytecode[pc + 1];
    switch (op) {
    case CONST:
      PUSH(code->consts[arg]);
      break;
    case ARG:
      assert(arg < nargs && "out of bounds args");
      PUSH(args[arg]);
      break;
    case ADD: {
      Object right = POP();
      Object left = POP();
      CachedValue cached = CACHE_AT(pc);
      Method method = cached.value;
      if (method == NULL || cached.key != left.type) {
        fprintf(stderr, "updating cache at %d\n", pc);
        method = lookup_method(left.type, kAdd);
        assert(method != NULL);
        CACHE_AT(pc) = (CachedValue){.key = left.type, .value = method};
      } else {
        fprintf(stderr, "using cached value at %d\n", pc);
      }
      Object result = (*method)(left, right);
      assert(result.type != kError);
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
  byte bytecode[] = {/*0:*/ ARG,   0,
                     /*2:*/ ARG,   1,
                     /*4:*/ ADD,   0,
                     /*6:*/ PRINT, 0,
                     /*8:*/ HALT,  0};
  Object consts[] = {
      new_int(3),
      new_int(4),
      new_str("szechuan "),
      new_str("broccoli"),
  };
  Object int_args[] = {
      new_int(5),
      new_int(10),
  };
  Object str_args[] = {
      new_str("hello "),
      new_str("world"),
  };
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize, consts);
#define ARRAYSIZE(ARR) (sizeof(ARR) / sizeof(ARR)[0])
  eval_code(&code, int_args, ARRAYSIZE(int_args));
  eval_code(&code, int_args, ARRAYSIZE(int_args));
  eval_code(&code, str_args, ARRAYSIZE(str_args));
  eval_code(&code, str_args, ARRAYSIZE(str_args));
  free(code.caches);
}
