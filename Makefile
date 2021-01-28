LDFLAGS = -lm
interpreter: interpreter.c
	$(CC) interpreter.c $(LDFLAGS) -o interpreter

make clean:
	rm interpreter
