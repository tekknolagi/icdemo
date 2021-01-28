#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FORCE_INLINE __attribute__((always_inline)) inline

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
  // Specialized ADD for integers.
  ADD_INT,
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

#define STACK_SIZE 100

typedef struct {
  Object stack_array[STACK_SIZE];
  Object *stack;
  Code *code;
  int pc;
} State;

static FORCE_INLINE void push(State *state, Object value) {
  CHECK(state->stack - state->stack_array < STACK_SIZE && "stack overflow");
  *state->stack++ = value;
}

static FORCE_INLINE Object pop(State *state) {
  CHECK(state->stack > state->stack_array && "stack underflow");
  return *--state->stack;
}

static FORCE_INLINE Object peek(State *state, int pos) {
  CHECK(state->stack - pos >= state->stack_array && "stack underflow");
  return state->stack[-pos];
}

void init_state(State *state, Code *code) {
  state->pc = 0;
  state->stack = state->stack_array;
  state->code = code;
}

void eval_code_uncached(Code *code, Object *args, int nargs) {
  State state;
  init_state(&state, code);
  while (true) {
    Opcode op = code->bytecode[state.pc];
    byte arg = code->bytecode[state.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&state, args[arg]);
        break;
      case ADD: {
        Object right = pop(&state);
        Object left = pop(&state);
        Method method = lookup_method(left.type, kAdd);
        Object result = (*method)(left, right);
        push(&state, result);
        break;
      }
      case PRINT: {
        Object obj = pop(&state);
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
    state.pc += kBytecodeSize;
  }
}

static FORCE_INLINE CachedValue cache_at(State *state) {
  return state->code->caches[state->pc / kBytecodeSize];
}

static FORCE_INLINE void cache_at_put(State *state, CachedValue value) {
  state->code->caches[state->pc / kBytecodeSize] = value;
}

void do_add_cached(State *state) {
  Object right = pop(state);
  Object left = pop(state);
  CachedValue cached = cache_at(state);
  Method method = cached.value;
  if (method == NULL || cached.key != left.type) {
    fprintf(stderr, "updating cache at %d\n", state->pc);
    method = lookup_method(left.type, kAdd);
    cache_at_put(state, (CachedValue){.key = left.type, .value = method});
  } else {
    fprintf(stderr, "using cached value at %d\n", state->pc);
  }
  Object result = (*method)(left, right);
  push(state, result);
}

void eval_code_cached(Code *code, Object *args, int nargs) {
  State state;
  init_state(&state, code);
  while (true) {
    Opcode op = code->bytecode[state.pc];
    byte arg = code->bytecode[state.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&state, args[arg]);
        break;
      case ADD: {
        do_add_cached(&state);
        break;
      }
      case PRINT: {
        Object obj = pop(&state);
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
    state.pc += kBytecodeSize;
  }
}

void do_add_int(State *state) {
  Object right = pop(state);
  Object left = pop(state);
  // Assume int
  Object result = int_add(left, right);
  push(state, result);
}

void eval_code_quickening(Code *code, Object *args, int nargs) {
  State state;
  init_state(&state, code);
  while (true) {
    Opcode op = code->bytecode[state.pc];
    byte arg = code->bytecode[state.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&state, args[arg]);
        break;
      case ADD: {
        if (peek(&state, 2).type == kInt) {
          // Rewrite to specialized handler
          fprintf(stderr, "rewriting ADD to ADD_INT at %d\n", state.pc);
          code->bytecode[state.pc] = ADD_INT;
          do_add_int(&state);
          break;
        }
        do_add_cached(&state);
        break;
      }
      case ADD_INT: {
        if (peek(&state, 2).type != kInt) {
          // Rewrite to generic handler
          code->bytecode[state.pc] = ADD;
          do_add_cached(&state);
          break;
        }
        do_add_int(&state);
        break;
      }
      case PRINT: {
        Object obj = pop(&state);
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
    state.pc += kBytecodeSize;
  }
}

Code new_code(byte *bytecode, int num_opcodes) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.caches = calloc(num_opcodes, sizeof *result.caches);
  return result;
}

int main(int argc, char **argv) {
  void (*eval)() = eval_code_uncached;
  if (argc == 2) {
    const char *mode = argv[1];
    if (strcmp(mode, "uncached") == 0) {
      eval = eval_code_uncached;
    } else if (strcmp(mode, "cached") == 0) {
      eval = eval_code_cached;
    } else if (strcmp(mode, "quickening") == 0) {
      eval = eval_code_quickening;
    } else {
      fprintf(stderr, "Usage: ./interpreter [uncached|cached|quickening]\n");
      return EXIT_FAILURE;
    }
  } else if (argc > 2) {
    fprintf(stderr, "Usage: ./interpreter [uncached|cached|quickening]\n");
    return EXIT_FAILURE;
  }
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
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
}
