#include <cstdio>

#include "foo.h"

Foo do_add(Foo left, Foo right) { return left.add(right); }

int main() {
  Foo left(3);
  Foo right(4);
  Foo result = do_add(left, right);
  printf("result: %d\n", result.value());
  return 0;
}
