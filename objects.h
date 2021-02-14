#pragma once

#include <inttypes.h>

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

#define FORCE_INLINE __attribute__((always_inline)) inline

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

Object* object_from_address(HeapObject* obj) {
  return (Object*)((uword)obj | kHeapObjectTag);
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
  return (word)obj >> kIntegerShift;
}

const char* object_as_str(Object* obj) {
  CHECK(object_is_str(obj));
  return object_address(obj)->str_value;
}

Object* new_int(word value) {
  CHECK(value < INTEGER_MAX && "too big");
  CHECK(value > INTEGER_MIN && "too small");
  return (Object*)((uword)value << kIntegerShift);
}

Object* new_str(const char* value) {
  HeapObject* result = (HeapObject*)malloc(sizeof *result);
  CHECK(result != NULL && "could not allocate object");
  *result = (HeapObject){.type = kStr, .str_value = value};
  return object_from_address(result);
}

Object* int_add(Object* left, Object* right) {
  CHECK(object_is_int(left));
  CHECK(object_is_int(right));
  return new_int(object_as_int(left) + object_as_int(right));
}

Object* int_print(Object* obj) {
  CHECK(object_is_int(obj));
  fprintf(stderr, "int: %ld\n", object_as_int(obj));
  return obj;
}

Object* str_add(Object* left, Object* right) {
  CHECK(object_is_str(left));
  CHECK(object_is_str(right));
  word result_size =
      strlen(object_as_str(left)) + strlen(object_as_str(right)) + 1;
  char* result = (char*)malloc(result_size);
  strcpy(result, object_as_str(left));
  strcat(result, object_as_str(right));
  return new_str(result);
}

Object* str_print(Object* obj) {
  CHECK(object_is_str(obj));
  fprintf(stderr, "str: \"%s\"\n", object_as_str(obj));
  return obj;
}
