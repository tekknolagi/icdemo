LDFLAGS = -lm
interpreter: interpreter.c
	$(CC) $(CFLAGS) interpreter.c $(LDFLAGS) -o interpreter

make clean:
	rm interpreter
