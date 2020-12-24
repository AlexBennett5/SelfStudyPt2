CC = gcc
CFLAGS = -g -Wall -fsanitize=address
TARGET = sniffer
DEPS = sniffer.h

%.o: %.c $(DEPS)
	$(CC) -c -o $@ $< $(CFLAGS) -lpcap

$(TARGET): $(TARGET).o
	$(CC) -o $(TARGET) $(TARGET).o $(CFLAGS) -lpcap

clean:
	rm -f *.o $(TARGET)
