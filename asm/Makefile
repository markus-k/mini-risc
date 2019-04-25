SOURCES = asm.c
OBJECTS = $(SOURCES:.c=.o)

CFLAGS = -Wall

all: asm

asm: $(OBJECTS)
	$(CC) -o $@ $^ $(LDFLAGS)

%.o: %.c
	$(CC) -c -o $@ $< $(CFLAGS)

.PHONY: clean
clean:
	rm -f asm *.o
