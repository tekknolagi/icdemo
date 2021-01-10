#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO(max): Consider writing this in Rust or Nim for some extra reader
// appeal.

typedef enum {
  // Load a constant from the constant array at index `arg'.
  CONST,
  // Load a value from the arguments array at index `arg'.
  ARG,
  // Add stack[-2] + stack[-1].
  ADD,
  // Pop the top of the stack and print it.
  PRINT,
  // Halt the machine.
  HALT,
} Bytecode;

typedef enum {
  kInt,
  kStr,
} ObjectType;

typedef struct {
  ObjectType type;
  union {
    const char *str_value;
    int int_value;
  };
} Object;

Object new_int(int value) {
  return (Object){.type = kInt, .int_value = value};
}

Object new_str(const char *value) {
  return (Object){.type = kStr, .str_value = value};
}

typedef enum {
  kAdd,
  kPrint,

  kUnknownSymbol = kPrint + 1,
} Symbol;

// Note: this takes advantage of the fact that in C, not putting anything
// between the parentheses means that this function can take any number of
// arguments.
typedef Object (*Method)();

typedef struct {
  Symbol name;
  Method method;
} MethodDefinition;

typedef unsigned char byte;

typedef struct {
  ObjectType key;
  Method value;
} CachedValue;

#define CHECK(cond) \
  do {              \
    if (!(cond)) {  \
      abort();      \
    }               \
  } while (0)

Object int_add(Object left, Object right) {
  CHECK(left.type == kInt);
  CHECK(right.type == kInt);
  return new_int(left.int_value + right.int_value);
}

Object int_print(Object obj) {
  CHECK(obj.type == kInt);
  fprintf(stderr, "int: %d\n", obj.int_value);
  return obj;
}

Object str_add(Object left, Object right) {
  CHECK(left.type == kStr);
  CHECK(right.type == kStr);
  int result_size = strlen(left.str_value) + strlen(right.str_value) + 1;
  char *result = malloc(result_size);
  strcpy(result, left.str_value);
  strcat(result, right.str_value);
  return new_str(result);
}

Object str_print(Object obj) {
  CHECK(obj.type == kStr);
  fprintf(stderr, "str: \"%s\"\n", obj.str_value);
  return obj;
}

static const MethodDefinition kIntMethods[] = {
    {kAdd, int_add},
    {kPrint, int_print},
    {kUnknownSymbol, NULL},
};

static const MethodDefinition kStrMethods[] = {
    {kAdd, str_add},
    {kPrint, str_print},
    {kUnknownSymbol, NULL},
};

static const MethodDefinition *kTypes[] = {
    [kInt] = kIntMethods,
    [kStr] = kStrMethods,
};

#define ARRAYSIZE(ARR) (sizeof(ARR) / sizeof(ARR)[0])

Method lookup_method(ObjectType type, Symbol name) {
  CHECK(type < ARRAYSIZE(kTypes) && "out of bounds type");
  const MethodDefinition *table = kTypes[type];
  for (int i = 0; table[i].method != NULL; i++) {
    if (table[i].name == name) {
      return table[i].method;
    }
  }
  CHECK(false && "could not find method");
}

typedef struct {
  // Array of `num_opcodes' (op, arg) pairs (total size `num_opcodes' * 2).
  byte *bytecode;
  int num_opcodes;
  // Array of `num_consts' constant values.
  Object *consts;
  int num_consts;
  // Array of `num_opcodes' elements.
  CachedValue *caches;
} Code;

Code new_code(byte *bytecode, int num_opcodes, Object *consts,
              int num_consts) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.consts = consts;
  result.num_consts = num_consts;
  result.caches = calloc(num_opcodes, sizeof *result.caches);
  return result;
}

static unsigned kBytecodeSize = 2;

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
        CHECK(arg < code->num_consts && "out of bounds const");
        PUSH(code->consts[arg]);
        break;
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
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
          CACHE_AT(pc) = (CachedValue){.key = left.type, .value = method};
        } else {
          fprintf(stderr, "using cached value at %d\n", pc);
        }
        Object result = (*method)(left, right);
        PUSH(result);
        break;
      }
      case PRINT: {
        Object obj = POP();
        Method method = lookup_method(obj.type, kPrint);
        (*method)(obj);
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
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize, consts,
                       ARRAYSIZE(consts));
  eval_code(&code, int_args, ARRAYSIZE(int_args));
  eval_code(&code, int_args, ARRAYSIZE(int_args));
  eval_code(&code, str_args, ARRAYSIZE(str_args));
  eval_code(&code, str_args, ARRAYSIZE(str_args));
}
