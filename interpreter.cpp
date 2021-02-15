#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <xbyak/xbyak.h>

#include "objects.h"

#define __ as->

// TODO(max): Consider writing this in Rust or Nim for some extra reader
// appeal.

typedef enum {
  kAdd,
  kPrint,
  kLessThan,

  kUnknownSymbol,
} Symbol;

// Note: this takes advantage of the fact that in C, not putting anything
// between the parentheses means that this function can take any number of
// arguments.
typedef Object* (*Method)();
typedef Object* (*Method1)(Object*);
typedef Object* (*Method2)(Object*, Object*);

typedef struct {
  Symbol name;
  Method method;
} MethodDefinition;

static const MethodDefinition kIntMethods[] = {
    {kAdd, (Method)int_add},
    {kPrint, (Method)int_print},
    {kUnknownSymbol, NULL},
};

static const MethodDefinition kStrMethods[] = {
    {kAdd, (Method)str_add},
    {kPrint, (Method)str_print},
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
  int num_opcodes;
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
  CHECK(type < ARRAYSIZE(kTypes) && "out of bounds type");
  const MethodDefinition* table = kTypes[type];
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
  Object* stack_array[STACK_SIZE];
  Object** stack;
  Code* code;
  int pc;
  Object** args;
} Frame;

static FORCE_INLINE void push(Frame* frame, Object* value) {
  CHECK(frame->stack - frame->stack_array < STACK_SIZE && "stack overflow");
  *frame->stack++ = value;
}

static FORCE_INLINE Object* pop(Frame* frame) {
  CHECK(frame->stack > frame->stack_array && "stack underflow");
  return *--frame->stack;
}

void init_frame(Frame* frame, Code* code, Object** args) {
  frame->pc = 0;
  // stack grows down
  frame->stack = frame->stack_array + STACK_SIZE;
  frame->code = code;
  frame->args = args;
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
  Method2 method = (Method2)lookup_method(object_type(left), kAdd);
  fprintf(stderr, "updating cache at %d\n", frame->pc);
  cache_at_put(frame, object_type(left), (Method)method);
  Object* result = (*method)(left, right);
  push(frame, result);
}

void do_add_int(Frame* frame, Object* left, Object* right) {
  Object* result = int_add(left, right);
  push(frame, result);
}

void eval_code_quickening(Code* code, Object** args, int nargs) {
  Frame frame;
  init_frame(&frame, code, args);
  while (true) {
    Opcode op = (Opcode)code->bytecode[frame.pc];
    byte arg = code->bytecode[frame.pc + 1];
    switch (op) {
      case ARG:
        CHECK(arg < nargs && "out of bounds arg");
        push(&frame, args[arg]);
        break;
      case ADD: {
        Object* right = pop(&frame);
        Object* left = pop(&frame);
        if (object_is_int(left)) {
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
        Method2 method = (Method2)cached.value;
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
        Method1 method = (Method1)lookup_method(object_type(obj), kPrint);
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

using Register = Xbyak::Reg64;
using Label = Xbyak::Label;
using Xbyak::util::bl;
using Xbyak::util::r8;
using Xbyak::util::r9;
using Xbyak::util::rax;
using Xbyak::util::rcx;
using Xbyak::util::cl;
using Xbyak::util::rdi;
using Xbyak::util::rdx;
using Xbyak::util::rsi;

static const Register kBCReg = rax;
static const Register kFrameReg = rdi;
static const Register kPCReg = rdx;
static const Xbyak::Reg8 kOpcodeReg = bl;
static const Xbyak::Reg8 kOpargReg = cl;
static const Register kOpargRegBig = rcx;
// Entrypoint receives arguments according to SystemV 64-bit ABI
static const Register kArgRegs[] = {rdi, rsi, rdx, rcx, r8, r9};

void emit_next_opcode(Xbyak::CodeGenerator* as, Label* dispatch) {
  __ add(kPCReg, kBytecodeSize);
  __ jmp(*dispatch);
}

void emit_assembly_interpreter(Xbyak::CodeGenerator* as) {
  using namespace Xbyak::util;
  //__ int3();
  // Load the frame from the first arg
  __ mov(kFrameReg, kArgRegs[0]);
  // Load the bytecode pointer into a register
  __ mov(kBCReg, qword[kFrameReg + offsetof(Frame, code)]);
  __ mov(kBCReg, qword[kBCReg + offsetof(Code, bytecode)]);
  // Initialize PC
  __ xor_(kPCReg, kPCReg);

  // while (true) {
  Label dispatch;
  __ L(dispatch);
  __ mov(kOpcodeReg, Xbyak::util::byte[kBCReg + kPCReg]);
  __ mov(kOpargReg, Xbyak::util::byte[kBCReg + kPCReg + 1]);

  // TODO(max): Make dispatch via a jump table instead of a series of
  // comparisons. With xbyak use putlabel/putL?
  /*
         Label labelTbl, L0, L1, L2;
         mov(rax, labelTbl);
         // rdx is an index of jump table
         jmp(ptr [rax + rdx * sizeof(void*)]);
      L(labelTbl);
         putL(L0);
         putL(L1);
         putL(L2);
      L(L0);
         ....
      L(L1);
         ....
  */
  Label handlers[NUM_OPCODES];
  for (int i = 0; i < NUM_OPCODES; i++) {
    __ cmp(kOpcodeReg, i);
    __ je(handlers[i]);
  }
  // Fall-through for invalid opcodes, I guess
  __ int3();
  __ mov(rax, -1);
  __ ret();

  __ L(handlers[ARG]);
  {
    Register r_scratch = r8;
    // Object** args = frame->args
    __ mov(r_scratch, qword[kFrameReg + offsetof(Frame, args)]);
    // push(args[arg])
    __ push(qword[r_scratch + kOpargRegBig]);
    emit_next_opcode(as, &dispatch);
  }

  __ L(handlers[ADD_INT]);
  {
    Register r_right = r8;
    Register r_left = r9;
    Label non_int;
    __ pop(r_right);
    __ pop(r_left);
    // Check both are ints
    static_assert(kIntegerTag == 0 && kIntegerShift == 1, "");
    __ test(r_left, kIntegerTagMask);
    __ jnz(non_int);
    __ test(r_right, kIntegerTagMask);
    __ jnz(non_int);
    __ add(r_left, r_right);
    __ push(r_left);
    emit_next_opcode(as, &dispatch);

    __ L(non_int);
    __ int3();
    __ mov(rax, -1);
    __ ret();
  }

  __ L(handlers[HALT]);
  {
    __ pop(rax);
    __ ret();
  }
}

// Signature of the generated function.
typedef Object* (*Func)(Frame* frame);

void eval_code_assembly(Code* code, Object** args) {
  Frame frame;
  init_frame(&frame, code, args);
  Xbyak::CodeGenerator as;
  emit_assembly_interpreter(&as);
  Func fn = as.getCode<Func>();
  Object* result = fn(&frame);
  if (object_type(result) != kInt) {
    fprintf(stderr, "error: expected int\n");
  }
  fprintf(stderr, "result: %d\n", object_as_int(result));
}

Code new_code(byte* bytecode, int num_opcodes) {
  Code result;
  result.bytecode = bytecode;
  result.num_opcodes = num_opcodes;
  result.caches = (CachedValue*)calloc(num_opcodes, sizeof *result.caches);
  return result;
}

int main(int argc, char** argv) {
  void (*eval)(Code*, Object**) = eval_code_assembly;
  // byte bytecode[] = {/*0:*/ ARG,   2,
  //                    /*2:*/ ARG,   0,
  //                    /*4:*/ ARG,   1,
  //                    /*6:*/ ADD,   0,
  //                    /*8:*/ LESS_THAN, 0,
  //                    /*10:*/ PRINT, 0,
  //                    /*12:*/ HALT,  0};
  // Object int_args[] = {
  //     new_int(5),
  //     new_int(10),
  //     new_int(20),
  // };
  // Object str_args[] = {
  //     new_str("hello "),
  //     new_str("world"),
  // };
  byte bytecode[] = {
      ARG, 0, ARG, 1, ADD_INT, 0, HALT, 0,
  };
  Object* int_args[] = {
      new_int(42),
      new_int(20),
  };
  fprintf(stderr, "args %p, %p\n", int_args[0], int_args[1]);
  Code code = new_code(bytecode, sizeof bytecode / kBytecodeSize);
  eval(&code, int_args);
  // eval(&code, int_args);
  // eval(&code, str_args, ARRAYSIZE(str_args));
  // eval(&code, str_args, ARRAYSIZE(str_args));
}
