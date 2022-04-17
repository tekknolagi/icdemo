EXTRA_FLAGS = -Wall -Wextra -Wpedantic
CFLAGS += $(EXTRA_FLAGS)
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS = -lm

interpreter: interpreter.c objects.h yjit_asm.h yjit_asm.c
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o interpreter

make clean:
	rm interpreter asm-interpreter
