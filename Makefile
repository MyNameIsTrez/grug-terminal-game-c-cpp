CC = gcc
CFLAGS = -Wall -Wextra -Werror -Wpedantic -Wfatal-errors -g -I. -Igrug -rdynamic -fsanitize=address,undefined -z execstack
SRCS = main.c grug/grug.c data.c game/human.c game/tool.c
TARGET = a.out

.PHONY: all clean

all: $(TARGET)

$(TARGET):
	$(CC) $(CFLAGS) $(SRCS) -o $(TARGET)

clean:
	rm -f $(TARGET)

