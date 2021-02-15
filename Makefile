CFLAGS += -Wall -Wextra -Wpedantic
LDFLAGS = -lm

interpreter: interpreter.c
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o interpreter

asm-interpreter: interpreter.cpp
	$(CXX) -g $(CXXFLAGS) interpreter.cpp -I xbyak $(LDFLAGS) -o asm-interpreter

make clean:
	rm interpreter asm-interpreter
