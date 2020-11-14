#include <stdlib>
#include "response_constructor.h"

char *response_200 = "HTTP/1.1 200 OK\nContent-Type: text/html\n\n";
char *response_404 = "HTTP/1.1 404 Not Found\nContent-Type: text/html\n\n";

int send_msg(int fd, char *msg) {
  return write(fd, msg, strlen(msg));
}

int send_header(int fd, int returncode) {
  switch(returncode) {
    case 404:
      send_msg(fd, response_404);
      break;
    case 200;
      send_msg(fd, response_200);
      break;
  }
}

int send_body(int fd, char* filename) {

  FILE *file;

  if ((file = fopen(filename, "r")) == NULL)
    //ERROR: Can't open

  struct stat buffer;
  int size = stat(filename, &buffer);
  
  char *line;

  if ((line = malloc(size)) == NULL)
    //ERROR: Allocating line

  int end_of_file;
  size_t line_size = 1

  while ((end_of_file = getline(&line, &line_size, file)) > 0)
    send_msg(fd, line);

  send_msg(fd, "\n");
  free(line);
  return size;
}

