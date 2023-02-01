LDFLAGS = -lm
interpreter: interpreter.c ujit_asm.c
	$(CC) -std=c11 -g $(CFLAGS) interpreter.c ujit_asm.c $(LDFLAGS) -o interpreter

make clean:
	rm interpreter
