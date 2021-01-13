#include "foo.h"

Foo Foo::add(Foo other) { return Foo(value_ + other.value_); }
