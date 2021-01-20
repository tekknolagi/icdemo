#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// TODO(max): Consider writing this in Rust or Nim for some extra reader
// appeal.

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

typedef enum {
  kAdd,
  kPrint,

  kUnknownSymbol,
} Symbol;

// Note: this takes advantage of the fact that in C, not putting anything
// between the parentheses means that this function can take any number of
// arguments.
typedef Object (*Method)();

typedef struct {
  Symbol name;
  Method method;
} MethodDefinition;

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

// I represent the type table differently from the method tables. I figure the
// method tables will be sparsely populated (not every type will implement
// every method) but the type table should contain every type. This is not an
// essential design decision.
static const MethodDefinition *kTypes[] = {
    [kInt] = kIntMethods,
    [kStr] = kStrMethods,
};

typedef unsigned char byte;

typedef struct {
  ObjectType key;
  Method value;
} CachedValue;

typedef struct {
  // Array of `num_opcodes' (op, arg) pairs (total size `num_opcodes' * 2).
  byte *bytecode;
  int num_opcodes;
  // Array of `num_opcodes' elements.
  CachedValue *caches;
} Code;

typedef enum {
  // Load a value from the arguments array at index `arg'.
  ARG,
  // Add stack[-2] + stack[-1].
  ADD,
  // Pop the top of the stack and print it.
  PRINT,
  // Halt the machine.
  HALT,
} Opcode;

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

static unsigned kBytecodeSize = 2;

void eval_code_uncached(Code *code, Object *args, int nargs) {
  int pc = 0;
#define STACK_SIZE 100
  Object stack_array[STACK_SIZE];
  Object *stack = stack_array;
#define PUSH(x) *stack++ = (x)
#define POP() *--stack
  while (true) {
    Opcode op = code->bytecode[pc];
    byte arg = code->bytecode[pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        PUSH(args[arg]);
        break;
      case ADD: {
        Object right = POP();
        Object left = POP();
        Method method = lookup_method(left.type, kAdd);
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

void eval_code_cached(Code *code, Object *args, int nargs) {
  int pc = 0;
#define STACK_SIZE 100
  Object stack_array[STACK_SIZE];
  Object *stack = stack_array;
#define PUSH(x) *stack++ = (x)
#define POP() *--stack
#define CACHE_AT(pc) code->caches[(pc) / kBytecodeSize]
  while (true) {
    Opcode op = code->bytecode[pc];
    byte arg = code->bytecode[pc + 1];
    switch (op) {
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

Code new_code(byte *bytecode, int num_opcodes) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.caches = calloc(num_opcodes, sizeof *result.caches);
  return result;
}

int main() {
  byte bytecode[] = {/*0:*/ ARG,   0,
                     /*2:*/ ARG,   1,
                     /*4:*/ ADD,   0,
                     /*6:*/ PRINT, 0,
                     /*8:*/ HALT,  0};
  Object int_args[] = {
      new_int(5),
      new_int(10),
  };
  Object str_args[] = {
      new_str("hello "),
      new_str("world"),
  };
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize);
  void (*eval)() = eval_code_cached;
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
}
