EXTRA_FLAGS = -Wall -Wextra -Wpedantic
CFLAGS += $(EXTRA_FLAGS)
CXXFLAGS += $(EXTRA_FLAGS)
LDFLAGS = -lm

interpreter: interpreter.c objects.h
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o interpreter

asm-interpreter: interpreter.cpp
	$(CXX) -g $(CXXFLAGS) interpreter.cpp -I xbyak $(LDFLAGS) -o asm-interpreter

make clean:
	rm interpreter asm-interpreter
