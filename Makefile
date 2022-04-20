EXTRA_FLAGS = -Wall -Wextra -Wpedantic
CFLAGS += $(EXTRA_FLAGS)
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS = -lm

build/interpreter: interpreter.c objects.h yjit_asm.h yjit_asm.c
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o $@

make clean:
	rm build/interpreter
