#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "objects.h"

typedef word intptr_t;
typedef uword uintptr_t;

// TODO(max): Consider writing this in Rust or Nim for some extra reader
// appeal.

typedef enum {
  kAdd,
  kPrint,

  kUnknownSymbol,
} Symbol;

// Note: this takes advantage of the fact that in C, not putting anything
// between the parentheses means that this function can take any number of
// arguments.
typedef Object* (*Method)();

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
static const MethodDefinition* kTypes[] = {
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
  byte* bytecode;
  word num_opcodes;
  // Array of `num_opcodes' elements.
  CachedValue* caches;
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
  // Halt the machine.
  HALT,
} Opcode;

#define ARRAYSIZE(ARR) (sizeof(ARR) / sizeof(ARR)[0])

Method lookup_method(ObjectType type, Symbol name) {
  CHECK(type < ARRAYSIZE(kTypes) && "out of bounds type");
  const MethodDefinition* table = kTypes[type];
  for (word i = 0; table[i].method != NULL; i++) {
    if (table[i].name == name) {
      return table[i].method;
    }
  }
  CHECK(false && "could not find method");
}

static unsigned kBytecodeSize = 2;

#define STACK_SIZE 100

typedef struct {
  Object* stack_array[STACK_SIZE];
  Object** stack;
  Code* code;
  word pc;
  Object** args;
  word nargs;
} Frame;

typedef void (*EvalFunc)(Frame* frame);

static FORCE_INLINE void frame_push(Frame* frame, Object* value) {
  CHECK(frame->stack > frame->stack_array && "stack overflow");
  *(--frame->stack) = value;
}

static FORCE_INLINE Object* frame_pop(Frame* frame) {
  CHECK(frame->stack + 1 <= frame->stack_array && "stack underflow");
  return *(frame->stack++);
}

void init_frame(Frame* frame, Code* code, Object** args, word nargs) {
  frame->pc = 0;
  // stack grows down
  frame->stack = frame->stack_array + STACK_SIZE;
  frame->code = code;
  frame->args = args;
  frame->nargs = nargs;
}

void eval_code_uncached(Frame* frame) {
  Code* code = frame->code;
  while (true) {
    Opcode op = code->bytecode[frame->pc];
    byte arg = code->bytecode[frame->pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < frame->nargs && "out of bounds arg");
        frame_push(frame, frame->args[arg]);
        break;
      case ADD: {
        Object* right = frame_pop(frame);
        Object* left = frame_pop(frame);
        Method method = lookup_method(object_type(left), kAdd);
        Object* result = (*method)(left, right);
        frame_push(frame, result);
        break;
      }
      case PRINT: {
        Object* obj = frame_pop(frame);
        Method method = lookup_method(object_type(obj), kPrint);
        (*method)(obj);
        break;
      }
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame->pc += kBytecodeSize;
  }
}

static FORCE_INLINE CachedValue cache_at(Frame* frame) {
  return frame->code->caches[frame->pc / kBytecodeSize];
}

static FORCE_INLINE void cache_at_put(Frame* frame, ObjectType key,
                                      Method value) {
  frame->code->caches[frame->pc / kBytecodeSize] =
      (CachedValue){.key = key, .value = value};
}

void add_update_cache(Frame* frame, Object* left, Object* right) {
  Method method = lookup_method(object_type(left), kAdd);
  fprintf(stderr, "updating cache at %ld\n", frame->pc);
  cache_at_put(frame, object_type(left), method);
  Object* result = (*method)(left, right);
  frame_push(frame, result);
}

void eval_code_cached(Frame* frame) {
  Code* code = frame->code;
  while (true) {
    Opcode op = code->bytecode[frame->pc];
    byte arg = code->bytecode[frame->pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < frame->nargs && "out of bounds arg");
        frame_push(frame, frame->args[arg]);
        break;
      case ADD: {
        Object* right = frame_pop(frame);
        Object* left = frame_pop(frame);
        CachedValue cached = cache_at(frame);
        Method method = cached.value;
        if (method == NULL || cached.key != object_type(left)) {
          add_update_cache(frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %ld\n", frame->pc);
        Object* result = (*method)(left, right);
        frame_push(frame, result);
        break;
      }
      case PRINT: {
        Object* obj = frame_pop(frame);
        Method method = lookup_method(object_type(obj), kPrint);
        (*method)(obj);
        break;
      }
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame->pc += kBytecodeSize;
  }
}

void do_add_int(Frame* frame, Object* left, Object* right) {
  Object* result = int_add(left, right);
  frame_push(frame, result);
}

void eval_code_quickening(Frame* frame) {
  Code* code = frame->code;
  while (true) {
    Opcode op = code->bytecode[frame->pc];
    byte arg = code->bytecode[frame->pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < frame->nargs && "out of bounds arg");
        frame_push(frame, frame->args[arg]);
        break;
      case ADD: {
        Object* right = frame_pop(frame);
        Object* left = frame_pop(frame);
        if (object_type(left) == kInt) {
          do_add_int(frame, left, right);
          code->bytecode[frame->pc] = ADD_INT;
          break;
        }
        add_update_cache(frame, left, right);
        code->bytecode[frame->pc] = ADD_CACHED;
        break;
      }
      case ADD_CACHED: {
        Object* right = frame_pop(frame);
        Object* left = frame_pop(frame);
        CachedValue cached = cache_at(frame);
        if (cached.key != object_type(left)) {
          add_update_cache(frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %ld\n", frame->pc);
        Method method = cached.value;
        Object* result = (*method)(left, right);
        frame_push(frame, result);
        break;
      }
      case ADD_INT: {
        Object* right = frame_pop(frame);
        Object* left = frame_pop(frame);
        if (object_type(left) != kInt) {
          add_update_cache(frame, left, right);
          code->bytecode[frame->pc] = ADD_CACHED;
          break;
        }
        do_add_int(frame, left, right);
        break;
      }
      case PRINT: {
        Object* obj = frame_pop(frame);
        Method method = lookup_method(object_type(obj), kPrint);
        (*method)(obj);
        break;
      }
      case HALT:
        return;
      default:
        fprintf(stderr, "unknown opcode %d\n", op);
        abort();
    }
    frame->pc += kBytecodeSize;
  }
}

Code new_code(byte* bytecode, word num_opcodes) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.caches = calloc(num_opcodes, sizeof *result.caches);
  return result;
}

int main(int argc, char** argv) {
  EvalFunc eval = eval_code_uncached;
  if (argc == 2) {
    const char* mode = argv[1];
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
  Object* int_args[] = {
      new_int(5),
      new_int(10),
  };
  Frame frame;
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize);
  init_frame(&frame, &code, int_args, ARRAYSIZE(int_args));
  eval(&frame);
  init_frame(&frame, &code, int_args, ARRAYSIZE(int_args));
  eval(&frame);
  Object* str_args[] = {
      new_str("hello "),
      new_str("world"),
  };
  init_frame(&frame, &code, str_args, ARRAYSIZE(str_args));
  eval(&frame);
  init_frame(&frame, &code, str_args, ARRAYSIZE(str_args));
  eval(&frame);
}
