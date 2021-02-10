#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FORCE_INLINE __attribute__((always_inline)) inline

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

// TODO(max): Consider writing this in Rust or Nim for some extra reader
// appeal.

typedef intptr_t word;
typedef uintptr_t uword;

typedef enum {
  kInt,
  kStr,
} ObjectType;

struct Object;
typedef struct Object Object;

typedef struct {
  ObjectType type;
  union {
    const char* str_value;
  };
} HeapObject;

// These constants are defined in a enum because the right hand side of a
// statement like
//     static const int kFoo = ...;
// must be a so-called "Integer Constant Expression". Compilers are required to
// support a certain set of these expressions, but are not required to support
// arbitrary arithmetic with other integer constants. Compilers such as gcc
// before gcc-8 just decided not to play this game, while gcc-8+ and Clang play
// just fine.
// Since this arithmetic with constant values works just fine for enums, make
// all these constants enum values instead.
// See https://twitter.com/tekknolagi/status/1328449329472835586 for more info.
enum {
  kBitsPerByte = 8,                         // bits
  kWordSize = sizeof(word),                 // bytes
  kBitsPerWord = kWordSize * kBitsPerByte,  // bits

  kIntegerTag = 0x0,      // 0b0
  kIntegerTagMask = 0x1,  // 0b1
  kIntegerShift = 1,
  kIntegerBits = kBitsPerWord - kIntegerShift,

  kHeapObjectTag = 0x1,      // 0b01
  kHeapObjectTagMask = 0x1,  // 0b01
};

// These are defined as macros because they will not work as static const int
// constants (per above explanation), and enum constants are only required to
// be an int wide (per ISO C).
#define INTEGER_MAX ((1LL << (kIntegerBits - 1)) - 1)
#define INTEGER_MIN (-(1LL << (kIntegerBits - 1)))

bool object_is_int(Object* obj) {
  return ((uword)obj & kIntegerTagMask) == kIntegerTag;
}

bool object_is_heap_object(Object* obj) {
  return ((uword)obj & kHeapObjectTagMask) == kHeapObjectTag;
}

HeapObject* object_address(Object* obj) {
  CHECK(object_is_heap_object(obj));
  return (HeapObject*)((uword)obj & ~kHeapObjectTagMask);
}

Object* object_new_heap_object(HeapObject* obj) {
  return (Object*)((uword)obj | kHeapObjectTagMask);
}

ObjectType object_type(Object* obj) {
  if (object_is_int(obj)) {
    return kInt;
  }
  return object_address(obj)->type;
}

bool object_is_str(Object* obj) { return object_type(obj) == kStr; }

word object_as_int(Object* obj) {
  CHECK(object_is_int(obj));
  return (uword)obj >> kIntegerShift;
}

const char* object_as_str(Object* obj) {
  CHECK(object_is_str(obj));
  return object_address(obj)->str_value;
}

Object* new_int(word value) {
  CHECK(value < INTEGER_MAX && "too big");
  CHECK(value > INTEGER_MIN && "too small");
  return (Object*)(value << kIntegerShift);
}

Object* new_str(const char* value) {
  HeapObject* result = malloc(sizeof *result);
  CHECK(result != NULL && "could not allocate object");
  *result = (HeapObject){.type = kStr, .str_value = value};
  return object_new_heap_object(result);
}

Object* int_add(Object* left, Object* right) {
  CHECK(object_is_int(left));
  CHECK(object_is_int(right));
  return new_int(object_as_int(left) + object_as_int(right));
}

Object* int_print(Object* obj) {
  CHECK(object_is_int(obj));
  fprintf(stderr, "int: %d\n", object_as_int(obj));
  return obj;
}

Object* str_add(Object* left, Object* right) {
  CHECK(object_is_str(left));
  CHECK(object_is_str(right));
  word result_size =
      strlen(object_as_str(left)) + strlen(object_as_str(right)) + 1;
  char* result = malloc(result_size);
  strcpy(result, object_as_str(left));
  strcat(result, object_as_str(right));
  return new_str(result);
}

Object* str_print(Object* obj) {
  CHECK(object_is_str(obj));
  fprintf(stderr, "str: \"%s\"\n", object_as_str(obj));
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
} Frame;

static FORCE_INLINE void push(Frame* frame, Object* value) {
  CHECK(frame->stack - frame->stack_array < STACK_SIZE && "stack overflow");
  *frame->stack++ = value;
}

static FORCE_INLINE Object* pop(Frame* frame) {
  CHECK(frame->stack > frame->stack_array && "stack underflow");
  return *--frame->stack;
}

void init_frame(Frame* frame, Code* code) {
  frame->pc = 0;
  frame->stack = frame->stack_array;
  frame->code = code;
}

void eval_code_uncached(Code* code, Object** args, word nargs) {
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
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        Method method = lookup_method(object_type(left), kAdd);
        Object* result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case PRINT: {
        Object* obj = pop(&frame);
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
    frame.pc += kBytecodeSize;
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
  fprintf(stderr, "updating cache at %d\n", frame->pc);
  cache_at_put(frame, object_type(left), method);
  Object* result = (*method)(left, right);
  push(frame, result);
}

void eval_code_cached(Code* code, Object** args, word nargs) {
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
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        CachedValue cached = cache_at(&frame);
        Method method = cached.value;
        if (method == NULL || cached.key != object_type(left)) {
          add_update_cache(&frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %d\n", frame.pc);
        Object* result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case PRINT: {
        Object* obj = pop(&frame);
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
    frame.pc += kBytecodeSize;
  }
}

void do_add_int(Frame* frame, Object* left, Object* right) {
  Object* result = int_add(left, right);
  push(frame, result);
}

void eval_code_quickening(Code* code, Object** args, word nargs) {
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
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        if (object_type(left) == kInt) {
          do_add_int(&frame, left, right);
          code->bytecode[frame.pc] = ADD_INT;
          break;
        }
        add_update_cache(&frame, left, right);
        code->bytecode[frame.pc] = ADD_CACHED;
        break;
      }
      case ADD_CACHED: {
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        CachedValue cached = cache_at(&frame);
        if (cached.key != object_type(left)) {
          add_update_cache(&frame, left, right);
          break;
        }
        fprintf(stderr, "using cached value at %d\n", frame.pc);
        Method method = cached.value;
        Object* result = (*method)(left, right);
        push(&frame, result);
        break;
      }
      case ADD_INT: {
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        if (object_type(left) != kInt) {
          add_update_cache(&frame, left, right);
          code->bytecode[frame.pc] = ADD_CACHED;
          break;
        }
        do_add_int(&frame, left, right);
        break;
      }
      case PRINT: {
        Object* obj = pop(&frame);
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
    frame.pc += kBytecodeSize;
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
  void (*eval)() = eval_code_uncached;
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
  Object* str_args[] = {
      new_str("hello "),
      new_str("world"),
  };
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize);
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, int_args, ARRAYSIZE(int_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
  eval(&code, str_args, ARRAYSIZE(str_args));
}
