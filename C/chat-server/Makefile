CC = gcc
CFLAGS = -g -Wall -fsanitize=address
TARGET = server
DEPS = server.h

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS)

$(TARGET): $(TARGET).o
	$(CC) -o $(TARGET) $(TARGET).o $(CFLAGS)

clean:
	rm -f *.o $(TARGET)
