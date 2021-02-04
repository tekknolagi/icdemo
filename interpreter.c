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

#if defined(NDEBUG)
#define CHECK
#else
#define CHECK(cond) \
  do {              \
    if (!(cond)) {  \
      abort();      \
    }               \
  } while (0)
#endif

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

Object int_less_than(Object left, Object right) {
  CHECK(left.type == kInt);
  CHECK(right.type == kInt);
  return new_int(left.int_value < right.int_value);
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
  kLessThan,

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
    {kLessThan, int_less_than},
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
  // Add stack[-2] + stack[-1] using cached method.
  ADD_CACHED,
  // Specialized ADD for integers.
  ADD_INT,
  // Pop the top of the stack and print it.
  PRINT,
  // Compute stack[-2] < stack[-1]. Push 1 if true, 0 if false.
  LESS_THAN,
  // Pop the top of the stack. Set the PC to `arg' if true (nonzero).
  POP_JUMP_IF_TRUE,
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
} Frame;

static FORCE_INLINE void push(Frame *frame, Object value) {
  CHECK(frame->stack - frame->stack_array < STACK_SIZE && "stack overflow");
  *frame->stack++ = value;
}

static FORCE_INLINE Object pop(Frame *frame) {
  CHECK(frame->stack > frame->stack_array && "stack underflow");
  return *--frame->stack;
}

void init_frame(Frame *frame, Code *code) {
  frame->pc = 0;
  frame->stack = frame->stack_array;
  frame->code = code;
}

void do_less_than(Frame *frame) {
  Object right = pop(frame);
  Object left = pop(frame);
  Method method = lookup_method(left.type, kLessThan);
  Object result = (*method)(left, right);
  push(frame, result);
}

void eval_code_uncached(Code *code, Object *args, int nargs) {
  Frame frame;
  init_frame(&frame, code);
  while (true) {
    Opcode op = code->bytecode[frame.pc];
    byte arg = code->bytecode[frame.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&frame, args[arg]);
        break;
      case ADD: {
        Object right = pop(&frame);
        Object left = pop(&frame);
        Method method = lookup_method(left.type, kAdd);
        Object result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case PRINT: {
        Object obj = pop(&frame);
        Method method = lookup_method(obj.type, kPrint);
        (*method)(obj);
        break;
      }
      case LESS_THAN:
        do_less_than(&frame);
        break;
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame.pc += kBytecodeSize;
  }
}

static FORCE_INLINE CachedValue cache_at(Frame *frame) {
  return frame->code->caches[frame->pc / kBytecodeSize];
}

static FORCE_INLINE void cache_at_put(Frame *frame, ObjectType key,
                                      Method value) {
  frame->code->caches[frame->pc / kBytecodeSize] =
      (CachedValue){.key = key, .value = value};
}

void add_update_cache(Frame *frame, Object left, Object right) {
  Method method = lookup_method(left.type, kAdd);
  fprintf(stderr, "updating cache at %d\n", frame->pc);
  cache_at_put(frame, left.type, method);
  Object result = (*method)(left, right);
  push(frame, result);
}

void eval_code_cached(Code *code, Object *args, int nargs) {
  Frame frame;
  init_frame(&frame, code);
  while (true) {
    Opcode op = code->bytecode[frame.pc];
    byte arg = code->bytecode[frame.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&frame, args[arg]);
        break;
      case ADD: {
        Object right = pop(&frame);
        Object left = pop(&frame);
        CachedValue cached = cache_at(&frame);
        Method method = cached.value;
        if (method == NULL || cached.key != left.type) {
          add_update_cache(&frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %d\n", frame.pc);
        Object result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case PRINT: {
        Object obj = pop(&frame);
        Method method = lookup_method(obj.type, kPrint);
        (*method)(obj);
        break;
      }
      case LESS_THAN:
        do_less_than(&frame);
        break;
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame.pc += kBytecodeSize;
  }
}

void do_add_int(Frame *frame, Object left, Object right) {
  Object result = int_add(left, right);
  push(frame, result);
}

void eval_code_quickening(Code *code, Object *args, int nargs) {
  Frame frame;
  init_frame(&frame, code);
  while (true) {
    Opcode op = code->bytecode[frame.pc];
    byte arg = code->bytecode[frame.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&frame, args[arg]);
        break;
      case ADD: {
        Object right = pop(&frame);
        Object left = pop(&frame);
        if (left.type == kInt) {
          do_add_int(&frame, left, right);
          code->bytecode[frame.pc] = ADD_INT;
          break;
        }
        add_update_cache(&frame, left, right);
        code->bytecode[frame.pc] = ADD_CACHED;
        break;
      }
      case ADD_CACHED: {
        Object right = pop(&frame);
        Object left = pop(&frame);
        CachedValue cached = cache_at(&frame);
        if (cached.key != left.type) {
          add_update_cache(&frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %d\n", frame.pc);
        Method method = cached.value;
        Object result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case ADD_INT: {
        Object right = pop(&frame);
        Object left = pop(&frame);
        if (left.type != kInt) {
          add_update_cache(&frame, left, right);
          code->bytecode[frame.pc] = ADD_CACHED;
          break;
        }
        do_add_int(&frame, left, right);
        break;
      }
      case PRINT: {
        Object obj = pop(&frame);
        Method method = lookup_method(obj.type, kPrint);
        (*method)(obj);
        break;
      }
      case LESS_THAN:
        do_less_than(&frame);
        break;
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame.pc += kBytecodeSize;
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
  byte bytecode[] = {/*0:*/ ARG,   2,
                     /*2:*/ ARG,   0,
                     /*4:*/ ARG,   1,
                     /*6:*/ ADD,   0,
                     /*8:*/ LESS_THAN, 0,
                     /*10:*/ PRINT, 0,
                     /*12:*/ HALT,  0};
  Object int_args[] = {
      new_int(5),
      new_int(10),
      new_int(20),
  };
  Object str_args[] = {
      new_str("hello "),
      new_str("world"),
  };
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize);
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, int_args, ARRAYSIZE(int_args));
  // eval(&code, str_args, ARRAYSIZE(str_args));
  // eval(&code, str_args, ARRAYSIZE(str_args));
}
