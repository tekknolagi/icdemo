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

  NUM_OPCODES,
} Opcode;

#define ARRAYSIZE(ARR) (sizeof(ARR) / sizeof(ARR)[0])

Method lookup_method(ObjectType type, Symbol name) {
  CHECK(type < ARRAYSIZE(kTypes), "out of bounds type");
  const MethodDefinition* table = kTypes[type];
  for (word i = 0; table[i].method != NULL; i++) {
    if (table[i].name == name) {
      return table[i].method;
    }
  }
  CHECK(false, "could not find method");
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
  CHECK(frame->stack > frame->stack_array, "stack overflow");
  *(--frame->stack) = value;
}

static FORCE_INLINE Object* frame_pop(Frame* frame) {
  CHECK(frame->stack + 1 <= frame->stack_array + STACK_SIZE,
        "stack underflow");
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

void do_print(Object* obj) {
  Method method = lookup_method(object_type(obj), kPrint);
  (*method)(obj);
}

void eval_code_uncached(Frame* frame) {
  Code* code = frame->code;
  while (true) {
    Opcode op = code->bytecode[frame->pc];
    byte arg = code->bytecode[frame->pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < frame->nargs, "out of bounds arg");
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
        CHECK(arg < frame->nargs, "out of bounds arg");
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
        CHECK(arg < frame->nargs, "out of bounds arg");
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

__attribute__((noreturn)) void rb_bug(const char* msg) {
  fprintf(stderr, "Error: %s\n", msg);
  abort();
}

#include "yjit_asm.c"

static const x86opnd_t kBCReg = RAX;
static const x86opnd_t kFrameReg = R12;
static const x86opnd_t kPCReg = RDX;
static const x86opnd_t kOpcodeReg = BL;
static const x86opnd_t kOpargReg = CL;
static const x86opnd_t kOpargRegBig = RCX;
static const x86opnd_t kCalleeSavedRegs[] = {RBX, RSP, RBP, R12, R13, R14, R15};
static const x86opnd_t kUsedCalleeSavedRegs[] = {RBX, R12};
const word kNumCalleeSavedRegs = ARRAYSIZE(kUsedCalleeSavedRegs);
const int kPointerSize = sizeof(void*);
const word kFrameOffset = -kNumCalleeSavedRegs * kPointerSize;
const word kPaddingBytes = (kFrameOffset % 16) == 0 ? 0 : kPointerSize;
const word kNativeStackFrameSize = -kFrameOffset + kPaddingBytes;
// Entrypoint receives arguments according to SystemV 64-bit ABI
static const x86opnd_t kArgRegs[] = {RDI, RSI, RDX, RCX, R8, R9};

const uint32_t qword = 8 * kBitsPerByte;
const uint32_t dword = 4 * kBitsPerByte;

typedef struct {
  uint32_t index_;
  const char* name_;
  bool bound_;
  bool initialized_;
} Label;

void Label_new(Label* label, const char* name) {
  label->name_ = name;
  label->bound_ = false;
  label->initialized_ = false;
}

void Label_init(Label* label, codeblock_t* cb) {
  label->index_ = cb_new_label(cb, label->name_);
  label->initialized_ = true;
}

bool Label_is_initialized(Label* label) { return label->initialized_; }

uint32_t Label_index(Label* label) {
  CHECK(Label_is_initialized(label), "expected label to be initialized");
  return label->index_;
}

bool Label_is_bound(Label* label) { return label->bound_; }

void Label_bind(Label* label, codeblock_t* cb) {
  CHECK(Label_is_initialized(label), "expected label to be initialized");
  CHECK(!Label_is_bound(label), "expected label not to be bound");
  cb_write_label(cb, Label_index(label));
  label->bound_ = true;
}

void emit_next_opcode(codeblock_t* cb, Label* dispatch) {
  add(cb, kPCReg, imm_opnd(kBytecodeSize));
  jmp_label(cb, Label_index(dispatch));
}

void emit_restore_interpreter_state(codeblock_t* cb) {
  // Load the interpreter stack
  mov(cb, RSP, member_opnd(kFrameReg, Frame, stack));
  // Load the bytecode pointer into a register
  mov(cb, kBCReg, member_opnd(kFrameReg, Frame, code));
  mov(cb, kBCReg, member_opnd(kBCReg, Code, bytecode));
  // Load PC
  mov(cb, kPCReg, member_opnd(kFrameReg, Frame, pc));
}

// TODO(max): Figure out why this one and _at_end are different.
void emit_restore_native_stack(codeblock_t* cb) {
  // Save PC
  mov(cb, member_opnd(kFrameReg, Frame, pc), kPCReg);
  // Don't bother saving bytecode pointer
  // Save interpreter stack
  mov(cb, member_opnd(kFrameReg, Frame, stack), RSP);
  lea(cb, RSP, mem_opnd(qword, RBP, -kNativeStackFrameSize));
}

// TODO(max): Figure out why this one and above are different.
void emit_restore_native_stack_at_end(codeblock_t* cb) {
  mov(cb, member_opnd(kFrameReg, Frame, stack), RSP);
  lea(cb, RSP, mem_opnd(qword, RBP, -kNumCalleeSavedRegs * kPointerSize));
}

#define INIT(name)         \
  Label_new(&name, #name); \
  Label_init(&name, cb)
#define L(name) \
  Label name;   \
  INIT(name)
#define BIND(name) Label_bind(&name, cb)
#define B(name) \
  L(name);      \
  BIND(name)
#define OP(op, name)                    \
  do {                                  \
    if (!Label_is_initialized(&name)) { \
      Label_init(&name, cb);            \
    }                                   \
    op##_label(cb, Label_index(&name)); \
  } while (0)

__attribute__((noreturn)) void report_error(const char* msg) {
  fprintf(stderr, "Error from asm: %s\n", msg);
  abort();
}

void asm_error(codeblock_t* cb, const char* msg, Label* error) {
  emit_restore_native_stack(cb);
  mov(cb, kArgRegs[0], const_ptr_opnd(msg));
  jmp_label(cb, Label_index(error));
}

void emit_asm_interpreter(codeblock_t* cb) {
  // Prologue
  // Set up a frame and save callee-saved registers we'll use.
  push(cb, RBP);
  mov(cb, RBP, RSP);
  for (word i = 0; i < kNumCalleeSavedRegs; i++) {
    push(cb, kUsedCalleeSavedRegs[i]);
  }

  // Load the frame from the first arg
  mov(cb, kFrameReg, kArgRegs[0]);
  emit_restore_interpreter_state(cb);

  // while (true) {
  B(dispatch);
  mov(cb, kOpcodeReg,
      mem_opnd_sib(/*size=*/1 * kBitsPerByte, kBCReg, kPCReg, /*scale=*/1,
                   /*disp=*/0));
  mov(cb, kOpargReg,
      mem_opnd_sib(/*size=*/1 * kBitsPerByte, kBCReg, kPCReg, /*scale=*/1,
                   /*disp=*/1));

  L(error);

  Label handlers[NUM_OPCODES];
  for (word i = 0; i < NUM_OPCODES; i++) {
    cmp(cb, kOpcodeReg, imm_opnd(i));
    INIT(handlers[i]);
    je_label(cb, Label_index(&handlers[i]));
  }
  // Fall-through for invalid opcodes, I guess
  asm_error(cb, "invalid opcode", &error);

  {
    BIND(handlers[ARG]);
    x86opnd_t r_scratch = R8;
    // Object** args = frame->args
    mov(cb, r_scratch, member_opnd(kFrameReg, Frame, args));
    // push(args[arg])
    push(cb,
         mem_opnd_sib(qword, r_scratch, kOpargRegBig, /*scale=*/kPointerSize,
                      /*disp=*/0));
    emit_next_opcode(cb, &dispatch);
  }

  {
    BIND(handlers[ADD]);
    // TODO(max): Call to C function
    asm_error(cb, "unimplemented: ADD", &error);
  }

  {
    BIND(handlers[ADD_CACHED]);
    // TODO(max): Call to C function
    asm_error(cb, "unimplemented: ADD_CACHED", &error);
  }

  {
    BIND(handlers[ADD_INT]);
    x86opnd_t r_right = R8;
    x86opnd_t r_left = R9;
    pop(cb, r_right);
    pop(cb, r_left);
    // Check both are ints
    CHECK(kIntegerTag == 0 && kIntegerShift == 1, "unexpected int tag");
    test(cb, r_left, imm_opnd(kIntegerTagMask));
    L(non_int);
    jnz_label(cb, Label_index(&non_int));
    test(cb, r_right, imm_opnd(kIntegerTagMask));
    jnz_label(cb, Label_index(&non_int));
    add(cb, r_left, r_right);
    push(cb, r_left);
    emit_next_opcode(cb, &dispatch);

    BIND(non_int);
    // TODO(max): Call to C function to execute and rewrite opcode
    asm_error(cb, "expected an integer in ADD_INT", &error);
  }

  {
    BIND(handlers[PRINT]);
    // TODO(max): Call to C function
    // asm_error(cb, "unimplemented: PRINT", &error);
    pop(cb, kArgRegs[0]);
    mov(cb, RAX, const_ptr_opnd((void*)do_print));
    emit_restore_native_stack(cb);
    // TODO(max): Figure out stack alignment
    call(cb, RAX);
    emit_restore_interpreter_state(cb);
    emit_next_opcode(cb, &dispatch);
  }

  // Epilogue
  BIND(handlers[HALT]);
  emit_restore_native_stack(cb);
  for (word i = kNumCalleeSavedRegs - 1; i >= 0; i--) {
    pop(cb, kUsedCalleeSavedRegs[i]);
  }
  pop(cb, RBP);
  ret(cb);

  // Error case
  BIND(error);
  mov(cb, RAX, const_ptr_opnd((void*)report_error));
  call(cb, RAX);
  ud2(cb);
}

EvalFunc gen_asm_interpreter() {
  codeblock_t cb;
  const uintptr_t mem_size = 1024;
  uint8_t* mem = alloc_exec_mem(mem_size);
  cb_init(&cb, mem, mem_size);
  emit_asm_interpreter(&cb);
  cb_link_labels(&cb);
  cb_mark_all_executable(&cb);
  return (EvalFunc)mem;
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
    } else if (strcmp(mode, "asm") == 0) {
      EvalFunc eval_code_assembly = gen_asm_interpreter();
      eval = eval_code_assembly;
    } else {
      fprintf(stderr,
              "Usage: ./interpreter [uncached|cached|quickening|asm]\n");
      return EXIT_FAILURE;
    }
  } else if (argc > 2) {
    fprintf(stderr, "Usage: ./interpreter [uncached|cached|quickening|asm]\n");
    return EXIT_FAILURE;
  }
  byte bytecode[] = {/*0:*/ ARG, 0,
                     /*2:*/ ARG, 1,
                     /*4:*/ ADD_INT, 0,
                     /*6:*/ PRINT, 0,
                     /*8:*/ HALT, 0};
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
  // // fprintf(stderr, "stack top: %ld\n", object_as_int(frame_pop(&frame)));
  // Object* str_args[] = {
  //     new_str("hello "),
  //     new_str("world"),
  // };
  // init_frame(&frame, &code, str_args, ARRAYSIZE(str_args));
  // eval(&frame);
  // init_frame(&frame, &code, str_args, ARRAYSIZE(str_args));
  // eval(&frame);
}
