CFLAGS += -Wall -Wextra -Wpedantic
LDFLAGS = -lm

interpreter: interpreter.c
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o interpreter

asm-interpreter: interpreter.cpp
	# Make sure to build asmjit first:
	#   mkdir -p asmjit/build
	#   cd asmjit/build
	#   cmake -GNinja ..
	#   ninja
	$(CXX) $(CXXFLAGS) interpreter.cpp -I asmjit/src/ -L asmjit/build/ $(LDFLAGS) -o asm-interpreter -lasmjit
	# Now run with:
	#   LD_LIBRARY_PATH=asmjit/build ./asm-interpreter

make clean:
	rm interpreter asm-interpreter
